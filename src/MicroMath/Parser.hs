{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module MicroMath.Parser where

import Control.Monad                  (void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Char                      (isAlphaNum)
import Data.List                      (sortOn)
import Data.Ord                       (Down (..))
import Data.Sequence                  (Seq, pattern (:<|), pattern Empty)
import Data.Sequence                  qualified as Seq
import Data.Set                       qualified as Set
import Data.String                    (IsString (..))
import Data.Text                      (Text)
import Data.Text                      qualified as Text
import Data.Void                      (Void)
import MicroMath.Expr                 (Expr (..), pattern (:@), pattern And,
                                       pattern Divide, pattern ExprInteger,
                                       pattern ExprReal, pattern ExprString,
                                       pattern ExprSymbol, pattern Or,
                                       pattern Plus, pattern Sequence,
                                       pattern Subtract, pattern Times)
import MicroMath.Expr                 qualified as Expr
import MicroMath.Symbol               (Symbol)
import Text.Megaparsec                (Parsec, Stream, Token, anySingle, choice,
                                       empty, eof, many, manyTill,
                                       notFollowedBy, optional, parseMaybe,
                                       satisfy, sepBy, single, token, try,
                                       (<|>))
import Text.Megaparsec.Char           (char, letterChar, space1, string)
import Text.Megaparsec.Char.Lexer     qualified as Lex

-- | Parse in two steps. First we parse Text into a stream of
-- Tok's. Then later parse a stream of Tok's into an Expr.
data Tok
  = TIdent  Symbol
  | TInt    Integer
  | TReal   Double -- TODO: Multiprecision
  | TPatVar (Maybe Symbol) BlankType (Maybe Symbol)
  | TSlot   (Maybe Integer) SlotType
  | TString Text
  | TLParen | TRParen -- ( )
  | TLBrack | TRBrack -- [ ]
  | TLBrace | TRBrace -- { }
  | TLAssoc | TRAssoc -- <| |>
  | TComma
  | TSemi
  | TOp Op
  deriving (Eq, Ord, Show)

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
  OpMap               -> "/@"  -- TODO
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
  OpQuestion          -> "?"   -- TODO
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
  OpAmpersand         -> "&"   -- TODO
  OpTilde             -> "~"   -- TODO
  OpPrefixApply       -> "@"
  OpColon             -> ":"

-- | Important: sorted in reverse order of length
opTextTable :: [(Op, Text)]
opTextTable =
  sortOn (Down . Text.length . snd)
  [ (op, opToText op) | op <- [minBound .. maxBound]]

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
parseNumber = lexeme $ try parseIntTok <|> try parseRealTok
  where
    parseIntTok = do
      n <- Lex.decimal
      notFollowedBy (char '.' <|> char '`' <|> char 'e')
      pure (TInt n)
    parseRealTok = do
      x <- Lex.float
      pure (TReal x)

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

-- | Insert OpPrefixApply before each left bracket
insertApply :: [Tok] -> [Tok]
insertApply ts = do
  t <- ts
  case t of
    TLBrack -> [TOp OpPrefixApply, TLBrack]
    _       -> pure t

lexAll :: TextParser [Tok]
lexAll = fmap insertApply (sc *> many parseTok <* eof)

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
  , tok (\case TReal x   -> Just (ExprReal x);    _ -> Nothing)
  , tok (\case TString s -> Just (ExprString s);  _ -> Nothing)
  , surround (TLParen, TRParen) parseExpr
  , ExprApp Expr.List        <$> surroundExprs (TLBrace, TRBrace)
  , ExprApp Expr.Sequence    <$> surroundExprs (TLBrack, TRBrack)
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

replaceHead :: Expr -> Expr -> Expr
replaceHead h (ExprApp _ cs) = ExprApp h cs
replaceHead _ expr           = expr

-- | A version of apply where we flatten an outer Sequence if it
-- exists. This is useful for normalizing expressions that appear from
-- the strategy in [Note: Application].
applyExprFlattenSequence :: Expr -> Expr -> Expr
applyExprFlattenSequence h (ExprApp Expr.Sequence args) = ExprApp h args
applyExprFlattenSequence h arg                          = Expr.unary h arg

opTable :: [[Operator TokParser Expr]]
opTable =
  [ [ binaryL OpPrefixApply     applyExprFlattenSequence
    ]
  , [ binaryL OpApply           (Expr.binary "Apply")
    ]
  , [ binaryR OpMap             (Expr.binary "Map")
    ]
  , [ prefix OpMinus (Expr.binary "Times" (ExprInteger (-1)))
    ]
  , [ binaryL OpPower           (Expr.binary "Power")
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
  ]

-- | Do some basic normalization to deal with quirks of the expression
-- parser. Replace (-) and (/) with their definitions in terms of
-- times and power. Flatten out Plus and Times and remove 0's and 1's,
-- respectively. Remove redundant Sequence's.
--
-- TODO: Some of these should be builtin rules...
normalize :: Expr -> Expr
normalize expr = case expr of
  ExprLit _ -> expr
  ExprSymbol _ -> expr
  h :@ (Sequence :@ args :<| Empty) ->
    normalize $ h :@ args
  Subtract :@ (e1 :<| e2 :<| Empty) ->
    normalize $ Expr.binary Plus e1  $ Expr.binary Times (ExprInteger (-1)) e2
  Divide :@ (e1 :<| e2 :<| Empty) ->
    normalize $ Expr.binary Times e1 $ Expr.binary Expr.Power e2 (ExprInteger (-1))
  Plus  :@ args -> flattenOneIdentity Plus (ExprInteger 0) args
  Times :@ args -> flattenOneIdentity Times (ExprInteger 1) args
  And   :@ args -> flattenOneIdentity And Expr.True args
  Or    :@ args -> flattenOneIdentity Or Expr.False args
  h :@ args ->
    normalize h :@ fmap normalize args
  where
    flattenOneIdentity h def args =
      let
        newArgs =
          Seq.filter (/= def) $
          Expr.flattenWithHead h $
          fmap normalize args
      in case newArgs of
        Empty         -> def
        (x :<| Empty) -> x
        _             -> h :@ newArgs

parseExpr :: TokParser Expr
parseExpr = normalize <$> makeExprParser parseTerm opTable

parseExprText :: Text -> Maybe Expr
parseExprText txt = do
  toks <- parseMaybe lexAll txt
  parseMaybe parseExpr toks

parseProgram :: TokParser [Expr]
parseProgram = many (parseExpr <* single TSemi)

parseProgramText :: Text -> Maybe [Expr]
parseProgramText prg = do
  toks <- parseMaybe lexAll prg
  parseMaybe parseProgram toks
