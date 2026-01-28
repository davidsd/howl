{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MicroMath.Parser
  ( normalizeParsedExpr
  , parseExprText
  , readExprFile
  ) where

import Control.Applicative            (some)
import Control.Monad                  (void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Control.Monad.IO.Class         (MonadIO, liftIO)
import Data.Char                      (isAlphaNum, isDigit)
import Data.List                      (sortOn)
import Data.Ord                       (Down (..))
import Data.Scientific                (Scientific, base10Exponent, coefficient)
import Data.Sequence                  (Seq, pattern (:<|), pattern Empty)
import Data.Sequence                  qualified as Seq
import Data.Set                       qualified as Set
import Data.String                    (IsString (..))
import Data.Text                      (Text)
import Data.Text                      qualified as Text
import Data.Text.IO                   qualified as Text
import Data.Void                      (Void)
import MicroMath.Expr                 (BigFloat, Expr (..), pattern (:@),
                                       pattern And, pattern Divide,
                                       pattern ExprBigFloat, pattern ExprDouble,
                                       pattern ExprInteger, pattern ExprNumeric,
                                       pattern ExprString, pattern ExprSymbol,
                                       pattern Optional, pattern Or,
                                       pattern Part, pattern Pattern,
                                       pattern CompoundExpression,
                                       pattern Plus, pattern Sequence,
                                       pattern Subtract, pattern Times)
import MicroMath.Expr                 qualified as Expr
import MicroMath.Symbol               (Symbol)
import MicroMath.Util                 (pattern Pair)
import Numeric.Rounded.Simple         qualified as Rounded
import Data.List                    (intercalate)
import Data.List.NonEmpty           (toList)
import Text.Megaparsec                (Parsec, Stream, Token, bundleErrors,
                                       bundlePosState, choice, empty, eof,
                                       errorBundlePretty, many, manyTill, match,
                                       notFollowedBy, optional, parse,
                                       pstateInput, satisfy, sepBy, single,
                                       token, try, (<|>))
import Text.Megaparsec.Char           (char, letterChar, space1, string)
import Text.Megaparsec.Char.Lexer     qualified as Lex
import Text.Megaparsec.Error         (ParseError, ParseErrorBundle,
                                      errorOffset)

-- | Parse in two steps. First we parse Text into a stream of
-- Tok's. Then later parse a stream of Tok's into an Expr.
data Tok
  = TIdent  Symbol
  | TInt    Integer
  | TReal   Double
  | TBigFloat BigFloatTok
  | TPatVar (Maybe Symbol) BlankType (Maybe Symbol)
  | TSlot   (Maybe Integer) SlotType
  | TString Text
  | TLParen | TRParen -- ( )
  | TLBrack | TRBrack -- [ ]
  | TLDblBrack | TRDblBrack -- [[ ]]
  | TLBrace | TRBrace -- { }
  | TLAssoc | TRAssoc -- <| |>
  | TComma
  | TOp Op
  deriving (Eq, Ord, Show)

newtype BigFloatTok = BigFloatTok BigFloat
  deriving (Eq, Ord)

instance Show BigFloatTok where
  show (BigFloatTok x) = Rounded.show' x

data BlankType = BlankTy | BlankSequenceTy | BlankNullSequenceTy
  deriving (Eq, Ord, Show)

data SlotType = SlotTy | SlotSequenceTy
  deriving (Eq, Ord, Show)

data Op
  = OpReplaceRepeated
  | OpMapApply
  | OpSameQ
  | OpUnsameQ
  | OpRepeatedNull
  | OpSpan
  | OpPostfixApply
  | OpReplaceAll
  | OpMap
  | OpApply
  | OpRule
  | OpRuleDelayed
  | OpSetDelayed
  | OpUpSet
  | OpUpSetDelayed
  | OpAnd
  | OpOr
  | OpEqual
  | OpUnequal
  | OpLessEqual
  | OpGreaterEqual
  | OpCondition
  | OpTagSetDelayed
  | OpDot
  | OpQuestion
  | OpBang
  | OpPower
  | OpTimes
  | OpDivide
  | OpPlus
  | OpMinus
  | OpSet
  | OpLess
  | OpGreater
  | OpAlternative
  | OpAmpersand
  | OpTilde
  | OpPrefixApply
  | OpBracketApply
  | OpPrefixPart
  | OpColon
  | OpSemi
  deriving (Eq, Ord, Show, Bounded, Enum)

opToText :: Op -> Text
opToText = \case
  OpReplaceRepeated   -> "//."
  OpMapApply          -> "@@@"
  OpSameQ             -> "==="
  OpUnsameQ           -> "=!="
  OpRepeatedNull      -> "..." -- TODO
  OpSpan              -> ";;"  -- TODO
  OpPostfixApply      -> "//"
  OpReplaceAll        -> "/."
  OpMap               -> "/@"
  OpApply             -> "@@"
  OpRule              -> "->"
  OpRuleDelayed       -> ":>"
  OpSetDelayed        -> ":="
  OpUpSet             -> "^="
  OpUpSetDelayed      -> "^:="
  OpAnd               -> "&&"
  OpOr                -> "||"
  OpEqual             -> "=="
  OpUnequal           -> "!="
  OpLessEqual         -> "<="
  OpGreaterEqual      -> ">="
  OpCondition         -> "/;"
  OpTagSetDelayed     -> "/:"
  OpDot               -> "."   -- TODO
  OpQuestion          -> "?"
  OpBang              -> "!"
  OpPower             -> "^"
  OpTimes             -> "*"
  OpDivide            -> "/"
  OpPlus              -> "+"
  OpMinus             -> "-"
  OpSet               -> "="
  OpLess              -> "<"
  OpGreater           -> ">"
  OpAlternative       -> "|"
  OpAmpersand         -> "&"
  OpTilde             -> "~"   -- TODO
  OpPrefixApply       -> "@"
  OpBracketApply      -> "<BracketApply>" -- internal only
  OpPrefixPart        -> "<Part>" -- internal only
  OpColon             -> ":"
  OpSemi              -> ";"

internalOps :: [Op]
internalOps = [OpBracketApply, OpPrefixPart]

-- | Important: sorted in reverse order of length
opTextTable :: [(Op, Text)]
opTextTable =
  sortOn (Down . Text.length . snd)
  [ (op, opToText op)
  | op <- [minBound .. maxBound]
  , not (op `elem` internalOps)
  ]

blankTypeToExpr :: BlankType -> Expr
blankTypeToExpr = \case
  BlankTy             -> Expr.Blank
  BlankSequenceTy     -> Expr.BlankSequence
  BlankNullSequenceTy -> Expr.BlankNullSequence

type TextParser = Parsec Void Text

sc :: TextParser ()
sc = Lex.space space1 empty (Lex.skipBlockComment "(*" "*)")

lexeme :: TextParser a -> TextParser a
lexeme = Lex.lexeme sc

parseOp :: TextParser Tok
parseOp = lexeme $ fmap TOp $ choice
  [ try (op <$ string opText) | (op, opText) <- opTextTable ]

parseString :: TextParser Tok
parseString = lexeme $ do
  _ <- char '"'
  s <- manyTill Lex.charLiteral (char '"')
  pure (TString (Text.pack s))

parseNumber :: TextParser Tok
parseNumber =
  lexeme $
    try parseBigFloatTok <|>
    try parseSciIntTok <|>
    try parseIntTok <|>
    try parseRealTok
  where
    decimalDigitsToBits :: Int -> Int
    decimalDigitsToBits digits =
      ceiling (fromIntegral digits * logBase 2 (10 :: Double))
    parseIntTok = do
      n <- Lex.decimal
      notFollowedBy (char '.' <|> char '`' <|> char 'e')
      pure (TInt n)
    parseBigFloatTok = do
      try parseBigFloatTick <|> parseBigFloatLong
    parseBigFloatTick = do
      n <- Lex.scientific
      _ <- char '`'
      prec <- (Lex.decimal :: TextParser Int)
      let r = toRational n
      pure . TBigFloat . BigFloatTok $
        Rounded.fromRational' Rounded.TowardNearest (decimalDigitsToBits prec) r
    parseBigFloatLong = do
      (txt, n) <- match Lex.scientific
      let
        (mantissa, _) = Text.break (\c -> c == 'e' || c == 'E') txt
        (_, fracWithDot) = Text.breakOn "." mantissa
        fracDigits = Text.takeWhile isDigit (Text.drop 1 fracWithDot)
        prec = Text.length fracDigits
      if Text.null fracWithDot || prec <= 16
        then fail "short decimal"
        else
          pure . TBigFloat . BigFloatTok $
          Rounded.fromRational' Rounded.TowardNearest (decimalDigitsToBits prec) (toRational n)
    parseSciIntTok = do
      (txt, n) <- match Lex.scientific
      let
        hasDot = Text.any (== '.') txt
        exp10 = base10Exponent n
      if hasDot || exp10 < 0
        then fail "not an integer scientific"
        else
          pure (TInt (sciToInteger n))
    parseRealTok = do
      x <- Lex.float
      pure (TReal x)

    sciToInteger :: Scientific -> Integer
    sciToInteger s =
      let c = coefficient s
          e = base10Exponent s
      in c * (10 ^ e)

specialCharMap :: [(String, Char)]
specialCharMap =
  [ ("Alpha",   '\x03B1'), ("Beta",    '\x03B2'), ("Gamma",   '\x03B3')
  , ("Delta",   '\x03B4'), ("Epsilon", '\x03B5'), ("Zeta",    '\x03B6')
  , ("Eta",     '\x03B7'), ("Theta",   '\x03B8'), ("Iota",    '\x03B9')
  , ("Kappa",   '\x03BA'), ("Lambda",  '\x03BB'), ("Mu",      '\x03BC')
  , ("Nu",      '\x03BD'), ("Xi",      '\x03BE'), ("Omicron", '\x03BF')
  , ("Pi",      '\x03C0'), ("Rho",     '\x03C1'), ("Sigma",   '\x03C3')
  , ("Tau",     '\x03C4'), ("Upsilon", '\x03C5'), ("Phi",     '\x03C6')
  , ("Chi",     '\x03C7'), ("Psi",     '\x03C8'), ("Omega",   '\x03C9')
  , ("CapitalAlpha",   '\x0391'), ("CapitalBeta",    '\x0392')
  , ("CapitalGamma",   '\x0393'), ("CapitalDelta",   '\x0394')
  , ("CapitalEpsilon", '\x0395'), ("CapitalZeta",    '\x0396')
  , ("CapitalEta",     '\x0397'), ("CapitalTheta",   '\x0398')
  , ("CapitalIota",    '\x0399'), ("CapitalKappa",   '\x039A')
  , ("CapitalLambda",  '\x039B'), ("CapitalMu",      '\x039C')
  , ("CapitalNu",      '\x039D'), ("CapitalXi",      '\x039E')
  , ("CapitalOmicron", '\x039F'), ("CapitalPi",      '\x03A0')
  , ("CapitalRho",     '\x03A1'), ("CapitalSigma",   '\x03A3')
  , ("CapitalTau",     '\x03A4'), ("CapitalUpsilon", '\x03A5')
  , ("CapitalPhi",     '\x03A6'), ("CapitalChi",     '\x03A7')
  , ("CapitalPsi",     '\x03A8'), ("CapitalOmega",   '\x03A9')
  ]

-- This is only a starter ident lexer.
-- WL symbols allow contexts with backticks, $, and more.
parseSymbol :: TextParser Symbol
parseSymbol = lexeme $ do
  first <- symbolStart
  rest <- many symbolChar
  pure (fromString (first:rest))
  where
    symbolStart = parseSpecialChar <|> letterChar <|> char '$'
    symbolChar =
      parseSpecialChar <|>
      satisfy (\c -> isAlphaNum c || c `elem` ("$`" :: String))

    parseSpecialChar = do
      _ <- string "\\["
      name <- some letterChar
      _ <- char ']'
      case lookup name specialCharMap of
        Just c -> pure c
        Nothing -> fail ("unknown special character: " <> name)

parseIdent :: TextParser Tok
parseIdent = TIdent <$> parseSymbol

parsePatVar :: TextParser Tok
parsePatVar = lexeme $ do
  name <- optional parseSymbol
  patType <- choice $ map try
    [ string "___" *> pure BlankNullSequenceTy
    , string "__"  *> pure BlankSequenceTy
    , string "_"   *> pure BlankTy
    ]
  headConstraint <- optional parseSymbol
  pure (TPatVar name patType headConstraint)

parseSlot :: TextParser Tok
parseSlot = lexeme $ do
  slotType <- choice $ map try
    [ string "##" *> pure SlotSequenceTy
    , string "#"  *> pure SlotTy
    ]
  n <- optional Lex.decimal
  pure $ TSlot n slotType

parsePunct :: TextParser Tok
parsePunct = lexeme $ choice
  [ TLParen       <$ char '('
  , TRParen       <$ char ')'
  , TLBrace       <$ char '{'
  , TRBrace       <$ char '}'
  , TLAssoc       <$ try (string "<|")
  , TRAssoc       <$ try (string "|>")
  , TLBrack       <$ char '['
  , TRBrack       <$ char ']'
  , TComma        <$ char ','
  ]

parseTok :: TextParser Tok
parseTok = choice
  [ parseString
  , parseNumber
  , try parsePatVar
  , parseIdent
  , parseSlot
  , parsePunct
  , parseOp
  ]

-- | [Note: Application] How do we handle application f[x] using an
-- expression parser. Conceptually, this is a binary
-- operator. However, it has a closing token. We handle it as follows:
--
-- 1) First replace each '[' with '@', '['. In other words, we stick a
--    (left-associative) prefix apply operator before every left
--    bracket.
-- 2) Parse [e1,...,en] as Sequence[e1,...,en].
--
-- The end result is that, e.g. f[x,y] gets parsed as f@Sequence[x,y],
-- which is correct, though a bit verbose.

-- | Insert OpPrefixApply before each left bracket, and OpPrefixPart
-- before each double left bracket.
insertApply :: [Tok] -> [Tok]
insertApply ts = do
  t <- ts
  case t of
    TLBrack    -> [TOp OpBracketApply, TLBrack]
    TLDblBrack -> [TOp OpPrefixPart, TLDblBrack]
    _          -> pure t

-- | Merge adjacent brackets into double brackets when they correspond
-- to a matching double-bracket pair.
mergeDoubleBrackets :: [Tok] -> [Tok]
mergeDoubleBrackets = go (0 :: Int)
  where
    go _ [] = []
    go depth (TLBrack:TLBrack:ts) = TLDblBrack : go (depth + 1) ts
    go depth (TRBrack:TRBrack:ts)
      | depth > 0 = TRDblBrack : go (depth - 1) ts
      | otherwise = TRBrack : go depth (TRBrack:ts)
    go depth (t:ts) = t : go depth ts

-- | Insert a trailing Null after a semicolon when the next token is a closing
-- token (or end of input), so compound expressions like "a;" parse as "a;Null".
insertNullAfterTrailingSemi :: [Tok] -> [Tok]
insertNullAfterTrailingSemi = go
  where
    go [] = []
    go [TOp OpSemi] = [TOp OpSemi, TIdent "Null"]
    go [t] = [t]
    go (t1:t2:ts)
      | t1 == TOp OpSemi && isClosingTok t2 =
          t1 : TIdent "Null" : go (t2:ts)
      | otherwise =
          t1 : go (t2:ts)

isClosingTok :: Tok -> Bool
isClosingTok = \case
  TRParen    -> True
  TRBrack    -> True
  TRDblBrack -> True
  TRBrace    -> True
  TRAssoc    -> True
  TComma     -> True
  _          -> False

-- | Insert OpTimes between tokens where whitespace should be interpreted as multiplication.
-- In Wolfram Language, adjacent terms like "2 x" or "f g" are multiplied.
insertTimes :: [Tok] -> [Tok]
insertTimes [] = []
insertTimes [t] = [t]
insertTimes (t1:t2:ts)
  | canEndMult t1 && canStartMult t2 = t1 : TOp OpTimes : insertTimes (t2:ts)
  | otherwise = t1 : insertTimes (t2:ts)

-- | Tokens that can appear on the left side of implicit multiplication
canEndMult :: Tok -> Bool
canEndMult = \case
  TIdent {}  -> True
  TInt _     -> True
  TReal _    -> True
  TBigFloat _ -> True
  TPatVar {} -> True
  TSlot {}   -> True
  TString _  -> True
  TRParen    -> True
  TRBrack    -> True
  TRDblBrack -> True
  TRBrace    -> True
  TRAssoc    -> True
  _          -> False

-- | Tokens that can appear on the right side of implicit multiplication
-- Note: TLBrack is NOT included because brackets denote function application
canStartMult :: Tok -> Bool
canStartMult = \case
  TIdent {}  -> True
  TInt _     -> True
  TReal _    -> True
  TBigFloat _ -> True
  TPatVar {} -> True
  TSlot {}   -> True
  TString _  -> True
  TLParen    -> True
  TLBrace    -> True
  TLAssoc    -> True
  _          -> False

lexAll :: TextParser [Tok]
lexAll =
  fmap (insertApply . insertTimes . insertNullAfterTrailingSemi . mergeDoubleBrackets)
    (sc *> many parseTok <* eof)

-- | Now we're ready to define parsers that consume Tok's. We will
-- build an expression parser, which means we only need to define
-- "term" (which can contain any construct that might be combined
-- using operators) and an operator table.

type TokParser = Parsec Void [Tok]

tok :: (Ord e, Stream s) => (Token s -> Maybe a) -> Parsec e s a
tok f = token f Set.empty

surround :: (Ord e, Stream s) => (Token s, Token s) -> Parsec e s a -> Parsec e s a
surround (l,r) p = single l *> p <* single r

tOp :: Op -> TokParser ()
tOp op = void $ (single (TOp op))

commaSep :: TokParser a -> TokParser [a]
commaSep p = p `sepBy` single TComma

surroundExprs :: (Tok, Tok) -> TokParser (Seq Expr)
surroundExprs (l,r) = Seq.fromList <$> surround (l,r) (commaSep parseExpr)

parseTerm :: TokParser Expr
parseTerm = choice
  [ tok (\case TInt n    -> Just (ExprInteger n); _ -> Nothing)
  , tok (\case TReal x   -> Just (ExprDouble x);  _ -> Nothing)
  , tok (\case TBigFloat (BigFloatTok x) -> Just (ExprBigFloat x); _ -> Nothing)
  , tok (\case TString s -> Just (ExprString s);  _ -> Nothing)
  , surround (TLParen, TRParen) parseExpr
  , ExprApp Expr.List        <$> surroundExprs (TLBrace, TRBrace)
  , ExprApp Expr.Sequence    <$> surroundExprs (TLBrack, TRBrack)
  , ExprApp Expr.Sequence    <$> surroundExprs (TLDblBrack, TRDblBrack)
  , ExprApp Expr.Association <$> surroundExprs (TLAssoc, TRAssoc)
  , tok $ \case
      TSlot m SlotTy         -> Just (Expr.unary Expr.Slot         (ExprInteger (maybe 1 id m)))
      TSlot m SlotSequenceTy -> Just (Expr.unary Expr.SlotSequence (ExprInteger (maybe 1 id m)))
      _                    -> Nothing
    -- We 'try' here because parsePatternVar might consume a symbol
    -- for the name of the pattern, but if the parser eventually
    -- fails, we need to restore the symbol so it can be parsed as a
    -- raw symbol.
  , try parsePatternVar
  , tok (\case TIdent s -> Just (ExprSymbol s); _ -> Nothing)
  ]

parsePatternVar :: TokParser Expr
parsePatternVar = tok $ \case
  TPatVar maybeName patHeadType maybeHeadConstraint -> Just $
    let
      nameFunction = case maybeName of
        Just name -> \p -> Expr.binary Expr.Pattern (ExprSymbol name) p
        Nothing   -> id
      args = case maybeHeadConstraint of
        Just h  -> Seq.singleton (ExprSymbol h)
        Nothing -> Empty
    in
      nameFunction $
      ExprApp (blankTypeToExpr patHeadType) args
  _ -> Nothing

binaryL :: Op -> (a -> a -> a) -> Operator TokParser a
binaryL  name f = InfixL  (f <$ tOp name)

binaryR :: Op -> (a -> a -> a) -> Operator TokParser a
binaryR  name f = InfixR  (f <$ tOp name)

prefix :: Op -> (a -> a) -> Operator TokParser a
prefix  name f = Prefix  (f <$ tOp name)

postfix :: Op -> (a -> a) -> Operator TokParser a
postfix name f = Postfix (f <$ tOp name)

-- | A version of apply where we flatten an outer Sequence if it
-- exists. This is useful for normalizing expressions that appear from
-- the strategy in [Note: Application].
applyExprFlattenSequence :: Expr -> Expr -> Expr
applyExprFlattenSequence h (ExprApp Expr.Sequence args) = ExprApp h args
applyExprFlattenSequence h arg                          = Expr.unary h arg

applyExprPartSequence :: Expr -> Expr -> Expr
applyExprPartSequence h (ExprApp Expr.Sequence args) = ExprApp Part (h :<| args)
applyExprPartSequence h arg = ExprApp Part (Seq.fromList [h, arg])

opTable :: [[Operator TokParser Expr]]
opTable =
  [ [ binaryL OpQuestion        (Expr.binary "PatternTest")
    ]
  , [ binaryL OpBracketApply    applyExprFlattenSequence
    ]
  , [ binaryR OpPrefixApply     applyExprFlattenSequence
    , binaryL OpPrefixPart      applyExprPartSequence
    ]
  , [ binaryL OpApply           (Expr.binary "Apply")
    ]
  , [ binaryR OpMap             (Expr.binary "Map")
    , binaryR OpMapApply        (Expr.binary "MapApply")
    ]
  , [ postfix OpBang            (Expr.unary "Factorial")
    ]
  , [ prefix OpMinus (Expr.binary "Times" (ExprInteger (-1)))
    ]
  , [ binaryR OpPower           (Expr.binary "Power")
    ]
  , [ binaryL OpTimes           (Expr.binary "Times")
    , binaryL OpDivide          (Expr.binary "Divide")
    ]
  , [ binaryL OpPlus            (Expr.binary "Plus")
    , binaryL OpMinus           (Expr.binary "Subtract")
    ]
  , [ binaryL OpOr              (Expr.binary "Or")
    , binaryL OpAnd             (Expr.binary "And")
    ]
  , [ binaryL OpLess            (Expr.binary "Less")
    , binaryL OpGreater         (Expr.binary "Greater")
    , binaryL OpLessEqual       (Expr.binary "LessEqual")
    , binaryL OpGreaterEqual    (Expr.binary "GreaterEqual")
    , binaryL OpEqual           (Expr.binary "Equal")
    , binaryL OpUnequal         (Expr.binary "Unequal")
    , binaryL OpSameQ           (Expr.binary "SameQ")
    , binaryL OpUnsameQ         (Expr.binary "UnsameQ")
    ]
  , [ binaryL OpAlternative     (Expr.binary "Alternatives")
    ]
  , [ binaryL OpColon           (Expr.binary "Pattern")
    ]
  , [ binaryL OpCondition       (Expr.binary "Test")
    ]
  , [ binaryR OpRuleDelayed     (Expr.binary "RuleDelayed")
    , binaryR OpRule            (Expr.binary "Rule")
    ]
  , [ binaryL OpReplaceAll      (Expr.binary "ReplaceAll")
    , binaryL OpReplaceRepeated (Expr.binary "ReplaceRepeated")
    ]
  , [ postfix OpAmpersand       (Expr.unary  "Function")
    ]
  , [ binaryL OpPostfixApply    (\e h -> Expr.unary h e)
    ]
  , [ binaryL OpTagSetDelayed   (Expr.binary "TagSetDelayed")
    ]
  , [ binaryR OpSet             (Expr.binary "Set")
    , binaryR OpSetDelayed      (Expr.binary "SetDelayed")
    , binaryR OpUpSet           (Expr.binary "UpSet")
    , binaryR OpUpSetDelayed    (Expr.binary "UpSetDelayed")
    ]
  , [ prefix OpQuestion         (Expr.unary "Help")
    ]
  , [ binaryL OpSemi            (Expr.binary CompoundExpression)
    ]
  ]

-- | [Note: Pattern vs Optional] In WL, both Pattern and Optional are
-- denoted by a colon. How are strings of colon-separated expressions
-- parsed? Firstly, colons are parsed in a left-associative way as
-- Pattern. Then a sequence of Patterns is transformed as follows:
--
-- Pattern[a,b] -> Pattern[a,b]
-- Pattern[Pattern[a,b],c] -> Optional[Pattern[a,b],c]
-- Pattern[Pattern[Pattern[a,b],c],d] -> Optional[Pattern[a,b],Pattern[c,d]]
-- Pattern[Pattern[Pattern[Pattern[a,b],c],d],e] -> Optional[Pattern[a,b],Optional[Pattern[c,d],e]]
--
-- In other words, we build a linked list of Optional[_,_] cells,
-- where the first element of the optional cell is always a
-- Pattern[_,_], while the second element can be a single expression,
-- a Pattern[_,_], or another Optional[_,_] cell.  We also have the
-- condition that the first element of any Pattern[_,_] (after this
-- transformation) must be a Symbol -- otherwise its a parse error.

-- | Transforms chains of Patterns as described above.
normalizePatternChains :: forall m. MonadFail m => Expr -> m Expr
normalizePatternChains expr = case expr of
  ExprLit _ -> pure expr
  ExprSymbol _ -> pure expr
  Pattern :@ Pair _ _ -> case collectPatternChain expr of
    []  -> fail "Invalid pattern expression"
    [_] -> fail "Invalid pattern expression"
    (x:y:rest) -> do
      p <- mkPattern x y
      buildOptionalChain p rest
  _ -> pure expr
  where
    mkPattern :: Expr -> Expr -> m Expr
    mkPattern a b = case a of
      ExprSymbol _ -> pure (Expr.binary Pattern a b)
      _            -> fail "Pattern name must be a symbol"

    collectPatternChain :: Expr -> [Expr]
    collectPatternChain = \case
      Pattern :@ Pair l r -> collectPatternChain l ++ [r]
      other               -> [other]

    buildOptionalChain :: Expr -> [Expr] -> m Expr
    buildOptionalChain h = \case
      []         -> pure h
      [a]        -> pure (Expr.binary Optional h a)
      (a:b:rest) -> do
        p <- mkPattern a b
        tailExpr <- buildOptionalChain p rest
        pure (Expr.binary Optional h tailExpr)

-- | Do some basic normalization to deal with quirks of the expression
-- parser. Replace (-) and (/) with their definitions in terms of
-- times and power. Flatten out Plus and Times and remove 0's and 1's,
-- respectively. Remove redundant Sequence's.
normalizeParsedExpr :: Expr -> Expr
normalizeParsedExpr expr = case expr of
  ExprLit _ -> expr
  ExprSymbol _ -> expr
  h :@ (Sequence :@ args :<| Empty) ->
    normalizeParsedExpr $ h :@ args
  Times :@ (ExprInteger (-1) :<| ExprNumeric n :<| Empty) -> ExprNumeric (negate n)
  Subtract :@ (e1 :<| e2 :<| Empty) ->
    normalizeParsedExpr $ Expr.binary Plus e1  $ Expr.binary Times (ExprInteger (-1)) e2
  Divide :@ (e1 :<| e2 :<| Empty) ->
    normalizeParsedExpr $ Expr.binary Times e1 $ Expr.binary Expr.Power e2 (ExprInteger (-1))
  Plus  :@ args -> flattenHead Plus  args
  Times :@ args -> flattenHead Times args
  And   :@ args -> flattenHead And   args
  Or    :@ args -> flattenHead Or    args
  CompoundExpression :@ args -> flattenHead CompoundExpression args
  h :@ args ->
    normalizeParsedExpr h :@ fmap normalizeParsedExpr args
  where
    flattenHead h args = h :@ Expr.flattenWithHead h (fmap normalizeParsedExpr args)

parseExpr :: TokParser Expr
parseExpr = do
  expr <- makeExprParser parseTerm opTable
  normalizePatternChains (normalizeParsedExpr expr)

parseExprTextWithPath :: FilePath -> Text -> Either String Expr
parseExprTextWithPath path txt =
  case parse lexAll path txt of
    Left bundle -> Left $ errorBundlePretty bundle
    Right toks ->
      case parse parseExpr path toks of
        Left bundle -> Left $ tokErrorBundlePretty bundle
        Right expr  -> Right expr

parseExprText :: Text -> Either String Expr
parseExprText = parseExprTextWithPath "<expr>"

readExprFile :: MonadIO m => FilePath -> m Expr
readExprFile path = liftIO $ do
  contents <- Text.readFile path
  case parseExprTextWithPath path contents of
    Left err   -> putStrLn err >> pure Expr.Null
    Right expr -> pure expr

tokErrorBundlePretty :: ParseErrorBundle [Tok] Void -> String
tokErrorBundlePretty bundle =
  let toks = pstateInput (bundlePosState bundle)
  in unlines (map (renderTokError toks) (toList (bundleErrors bundle)))

renderTokError :: [Tok] -> ParseError [Tok] Void -> String
renderTokError toks err =
  let
    off = errorOffset err
    before = 3
    after = 3
    start = max 0 (off - before)
    window = take (before + 1 + after) (drop start toks)
    markerIndex = off - start
    renderTok idx t
      | idx == markerIndex = ">>" <> show t <> "<<"
      | otherwise = show t
    rendered = case window of
      [] -> "<end of input>"
      _  -> intercalate " " (zipWith renderTok [0..] window)
  in unlines
    [ "Parse error at token offset " <> show off <> ":"
    , "  " <> show err
    , "  near: " <> rendered
    ]
