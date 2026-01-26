{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module MicroMath.Parser
  ( normalizeParsedExpr
  , parseExprText
  , parseCompoundExpressionText
  , readExprFile
  ) where

import Control.Monad                  (void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Control.Monad.IO.Class         (MonadIO, liftIO)
import Data.Char                      (isAlphaNum, isDigit)
import Data.List                      (sortOn)
import Data.Ord                       (Down (..))
import Data.Scientific                (Scientific, base10Exponent, coefficient)
import Data.Sequence                  (Seq, pattern (:<|), pattern Empty, (|>))
import Data.Sequence                  qualified as Seq
import Data.Set                       qualified as Set
import Data.String                    (IsString (..))
import Data.Text                      (Text)
import Data.Text                      qualified as Text
import Data.Text.IO                   qualified as Text
import Data.Void                      (Void)
import MicroMath.Expr                 (BigFloat, Expr (..), pattern (:@),
                                       pattern And, pattern Divide,
                                       pattern ExprBigFloat, pattern ExprNumeric,
                                       pattern ExprDouble, pattern ExprInteger,
                                       pattern ExprString, pattern ExprSymbol,
                                       pattern Or, pattern Part, pattern Plus,
                                       pattern Sequence, pattern Subtract,
                                       pattern Times)
import MicroMath.Expr                 qualified as Expr
import MicroMath.Symbol               (Symbol)
import MicroMath.Util                 (pattern Solo)
import Numeric.Rounded.Simple         qualified as Rounded
import Text.Megaparsec                (Parsec, Stream, Token, anySingle, choice,
                                       empty, eof, errorBundlePretty, many,
                                       manyTill, match, notFollowedBy, optional,
                                       parse, parseMaybe, satisfy, sepBy,
                                       single, token, try, (<|>))
import Text.Megaparsec.Char           (char, letterChar, space1, string)
import Text.Megaparsec.Char.Lexer     qualified as Lex

-- | Parse in two steps. First we parse Text into a stream of
-- Tok's. Then later parse a stream of Tok's into an Expr.
data Tok
  = TIdent  Symbol
  | TInt    Integer
  | TReal   Double -- TODO: Multiprecision
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
  | TSemi
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
  | OpPrefixPart
  | OpColon
  deriving (Eq, Ord, Show, Bounded, Enum)

opToText :: Op -> Text
opToText = \case
  OpReplaceRepeated   -> "//."
  OpMapApply          -> "@@@" -- TODO
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
  OpBang              -> "!"   -- TODO
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
  OpPrefixPart        -> "<Part>" -- internal only
  OpColon             -> ":"

-- | Important: sorted in reverse order of length
opTextTable :: [(Op, Text)]
opTextTable =
  sortOn (Down . Text.length . snd)
  [ (op, opToText op) | op <- [minBound .. maxBound], op /= OpPrefixPart]

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
      pure (TBigFloat (BigFloatTok (Rounded.fromRational' Rounded.TowardNearest (decimalDigitsToBits prec) r)))
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
          pure (TBigFloat (BigFloatTok (Rounded.fromRational' Rounded.TowardNearest (decimalDigitsToBits prec) (toRational n))))
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

-- This is only a starter ident lexer.
-- WL symbols allow contexts with backticks, $, and more.
parseSymbol :: TextParser Symbol
parseSymbol = lexeme $ do
  first <- letterChar <|> char '$'
  rest <- many (satisfy (\c -> isAlphaNum c || c `elem` ("$`" :: String)))
  pure (fromString (first:rest))

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
  , TSemi         <$ char ';'
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
    TLBrack    -> [TOp OpPrefixApply, TLBrack]
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
lexAll = fmap (insertApply . insertTimes . mergeDoubleBrackets) (sc *> many parseTok <* eof)

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
parsePatternVar = do
  TPatVar maybeName patHeadType maybeHeadConstraint <- anySingle
  maybeDefault <- optional (tOp OpColon *> parseExpr)
  let
    defFunction = case maybeDefault of
      Just def -> \p -> Expr.binary Expr.Optional p def
      Nothing  -> id
    nameFunction = case maybeName of
      Just name -> \p -> Expr.binary Expr.Pattern (ExprSymbol name) p
      Nothing   -> id
    args = case maybeHeadConstraint of
      Just h  -> Seq.singleton (ExprSymbol h)
      Nothing -> Empty
  pure $
    defFunction $
    nameFunction $
    ExprApp (blankTypeToExpr patHeadType) args

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
applyExprFlattenSequence h arg = Expr.unary h arg

applyExprPartSequence :: Expr -> Expr -> Expr
applyExprPartSequence h (ExprApp Expr.Sequence args) = ExprApp Part (h :<| args)
applyExprPartSequence h arg = ExprApp Part (Seq.fromList [h, arg])

opTable :: [[Operator TokParser Expr]]
opTable =
  [ [ binaryL OpPrefixApply     applyExprFlattenSequence
    , binaryL OpPrefixPart      applyExprPartSequence
    ]
  , [ binaryL OpApply           (Expr.binary "Apply")
    ]
  , [ binaryR OpMap             (Expr.binary "Map")
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
    , binaryL OpColon           (Expr.binary "Pattern")
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
  ]

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
  Plus  :@ args -> flattenOneIdentity Plus (ExprInteger 0) args
  Times :@ args -> flattenOneIdentity Times (ExprInteger 1) args
  And   :@ args -> flattenOneIdentity And Expr.True args
  Or    :@ args -> flattenOneIdentity Or Expr.False args
  h :@ args ->
    normalizeParsedExpr h :@ fmap normalizeParsedExpr args
  where
    flattenOneIdentity h def args =
      let
        newArgs =
          Seq.filter (/= def) $
          Expr.flattenWithHead h $
          fmap normalizeParsedExpr args
      in case newArgs of
        Empty         -> def
        (x :<| Empty) -> x
        _             -> h :@ newArgs

parseExpr :: TokParser Expr
parseExpr = normalizeParsedExpr <$> makeExprParser parseTerm opTable

parseExprText :: Text -> Maybe Expr
parseExprText txt = do
  toks <- parseMaybe lexAll txt
  parseMaybe parseExpr toks

parseCompoundExpression :: TokParser Expr
parseCompoundExpression = do
  (exprs, hasTrailing) <- getExpr Empty
  pure $ case (exprs, hasTrailing) of
    (Empty, _)         -> Expr.Null
    (Solo expr, False) -> expr
    (_        , True)  -> Expr.CompoundExpression :@ (exprs |> Expr.Null)
    (_        , False) -> Expr.CompoundExpression :@ exprs
  where
    getExpr exprs = optional parseExpr >>= \case
      Just expr -> getSemi (exprs |> expr)
      -- Has trailing semicolon
      Nothing   -> pure (exprs, True)
    getSemi exprs = optional (single TSemi) >>= \case
      Just _ -> getExpr exprs
      -- No trailing semicolon
      Nothing -> pure (exprs, False)

parseCompoundExpressionText :: FilePath -> Text -> Either String Expr
parseCompoundExpressionText path ce =
  case parse lexAll path ce of
    Left bundle -> Left $ errorBundlePretty bundle
    Right toks ->
      case parse parseCompoundExpression path toks of
        Left _     -> Left $ "Error parsing: " <> show ce
        Right expr -> Right expr

readExprFile :: MonadIO m => FilePath -> m Expr
readExprFile path = liftIO $ do
  contents <- Text.readFile path
  case parseCompoundExpressionText path contents of
    Left err   -> putStrLn err >> pure Expr.Null
    Right expr -> pure expr
