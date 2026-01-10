{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module MicroMath.Parser where

import Control.Monad                  (void)
import Control.Monad.Combinators.Expr
import Data.Char                      (isAlphaNum)
import Data.List                      (sortOn)
import Data.Ord                       (Down (..))
import Data.Scientific                (Scientific)
import Data.Set                       qualified as Set
import Data.Text                      (Text)
import Data.Text                      qualified as Text
import Data.Void
import MicroMath.Expr                 (Expr (..), Literal (..), Symbol (..))
import MicroMath.Expr                 qualified as Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer     qualified as Lex

-- | Parse in two steps. First we parse Text into a stream of
-- Tok's. Then later parse a stream of Tok's into an Expr.
data Tok
  = TIdent  Symbol
  | TInt    Integer
  | TReal   Scientific
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

data BlankType = Blank | BlankSequence | BlankNullSequence
  deriving (Eq, Ord, Show)

data SlotType = Slot | SlotSequence
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

blankTypeToSymbol :: BlankType -> Symbol
blankTypeToSymbol = \case
  Blank             -> "Blank"
  BlankSequence     -> "BlankSequence"
  BlankNullSequence -> "BlankNullSequence"

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
      notFollowedBy (char '.' <|> char '`' <|> char '*' <|> char 'e' <|> char 'E')
      pure (TInt n)
    parseRealTok = do
      x <- Lex.scientific
      pure (TReal x)

-- This is only a starter ident lexer.
-- WL symbols allow contexts with backticks, $, and more.
parseSymbol :: TextParser Symbol
parseSymbol = lexeme $ do
  first <- letterChar <|> char '$'
  rest <- many (satisfy (\c -> isAlphaNum c || c `elem` ("$`" :: String)))
  pure (MkSymbol (Text.pack (first:rest)))

parseIdent :: TextParser Tok
parseIdent = TIdent <$> parseSymbol

parsePatVar :: TextParser Tok
parsePatVar = lexeme $ do
  name <- optional parseSymbol
  patType <- choice $ map try
    [ string "___" *> pure BlankNullSequence
    , string "__"  *> pure BlankSequence
    , string "_"   *> pure Blank
    ]
  headConstraint <- optional parseSymbol
  pure (TPatVar name patType headConstraint)

parseSlot :: TextParser Tok
parseSlot = lexeme $ do
  slotType <- choice $ map try
    [ string "##" *> pure SlotSequence
    , string "#"  *> pure Slot
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

surroundExprs :: (Tok, Tok) -> TokParser [Expr]
surroundExprs (l,r) = surround (l,r) (commaSep parseExpr)

parseTerm :: TokParser Expr
parseTerm = choice
  [ tok (\case TInt n    -> Just (ExprAtom (LitInteger n)); _ -> Nothing)
  , tok (\case TReal x   -> Just (ExprAtom (LitReal x));    _ -> Nothing)
  , tok (\case TString s -> Just (ExprAtom (LitString s));  _ -> Nothing)
  , surround (TLParen, TRParen) parseExpr
  , ExprApp "List"        <$> surroundExprs (TLBrace, TRBrace)
  , ExprApp "Sequence"    <$> surroundExprs (TLBrack, TRBrack)
  , ExprApp "Association" <$> surroundExprs (TLAssoc, TRAssoc)
  , tok $ \case
      TSlot m Slot         -> Just (ExprApp "Slot" [maybe 1 fromInteger m])
      TSlot m SlotSequence -> Just (ExprApp "SlotSequence" [maybe 1 fromInteger m])
      _                    -> Nothing
    -- We 'try' here because parsePatternVar might consume a symbol
    -- for the name of the pattern, but if the parser eventually
    -- fails, we need to restore the symbol so it can be parsed as a
    -- raw symbol.
  , try parsePatternVar
  , tok (\case TIdent s -> Just (ExprAtom (LitSymbol s)); _ -> Nothing)
  ]

parsePatternVar :: TokParser Expr
parsePatternVar = do
  TPatVar maybeName patHeadType maybeHeadConstraint <- anySingle
  maybeDefault <- optional (tOp OpColon *> parseExpr)
  let
    patHead = ExprAtom (LitSymbol (blankTypeToSymbol patHeadType))
    defFunction = case maybeDefault of
      Just def -> \p -> ExprApp "Optional" [p, def]
      Nothing  -> id
    nameFunction = case maybeName of
      Just name -> \p -> ExprApp "Pattern" [ExprAtom (LitSymbol name), p]
      Nothing   -> id
    args = case maybeHeadConstraint of
      Just h  -> [ExprAtom (LitSymbol h)]
      Nothing -> []
  pure $
    defFunction $
    nameFunction $
    ExprApp patHead args

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
applyExpr :: Expr -> Expr -> Expr
applyExpr h (ExprApp "Sequence" args) = ExprApp h args
applyExpr h arg                       = ExprApp h [arg]

opTable :: [[Operator TokParser Expr]]
opTable =
  [ [ binaryL OpPrefixApply     applyExpr
    , binaryL OpApply           replaceHead
    ]
  , [ prefix OpMinus negate
    , prefix OpPlus  id
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
  , [ binaryL OpPostfixApply    (\e h -> ExprApp h [e])
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
normalize :: Expr -> Expr
normalize expr = case expr of
  ExprAtom _ -> expr
  ExprApp h [ExprApp "Sequence" args] ->
    normalize $ ExprApp h args
  ExprApp "Subtract" [e1, e2] ->
    normalize $ ExprApp "Plus" ([e1, ExprApp "Times" [-1, e2]])
  ExprApp "Divide" [e1, e2] ->
    normalize $ ExprApp "Times" [e1, ExprApp "Power" [e2, -1]]
  ExprApp "Plus" args ->
    ExprApp "Plus" $
    filter (/= 0) $
    Expr.flattenWithHead "Plus" $
    map normalize args
  ExprApp "Times" args ->
    ExprApp "Times" $
    filter (/= 1) $
    Expr.flattenWithHead "Times" $
    map normalize args
  ExprApp h args ->
    ExprApp (normalize h) (map normalize args)

parseExpr :: TokParser Expr
parseExpr = normalize <$> makeExprParser parseTerm opTable

parseProgram :: TokParser [Expr]
parseProgram = many (parseExpr <* single TSemi)

parseProgramText :: Text -> Maybe [Expr]
parseProgramText prg = do
  toks <- parseMaybe lexAll prg
  parseMaybe parseProgram toks
