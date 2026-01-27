{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module MicroMath.Pat
  ( Pat(..)
  , PatAppType(..)
  , SeqType(..)
  , outerNames
  , mapNames
  , addNames
  , patRootSymbol
  , patFromExpr
  ) where

import Control.Monad.State   (StateT, evalStateT, lift, state)
import Data.Foldable         qualified as Foldable
import Data.List             (intercalate)
import Data.Sequence         (Seq, pattern (:<|), pattern Empty)
import Data.String           (IsString (..))
import MicroMath.Expr        (Expr (..), Literal, binary, pattern (:@),
                              pattern Alternatives, pattern Blank,
                              pattern BlankNullSequence, pattern BlankSequence,
                              pattern ConfirmPatternTest, pattern Default,
                              pattern Optional, pattern Pattern,
                              pattern PatternTest, pattern Test)
import MicroMath.Expr.PPrint ()
import MicroMath.PPrint      (PPrint (..))
import MicroMath.Symbol      (Symbol)
import MicroMath.Util        (pattern Pair, pattern Solo)

{- | TODO:

Composite Patterns
- p..(Repeated), p...(RepeatedNull)
- Except
- Longest
- Shortest
- OptionsPattern, PatternSequence, Verbatim, HoldPattern
- OrderlessPatternSequence
- KeyValuePattern

Pattern Defaults
_:e (Optional) pattern that defaults to e if omitted
_. (Optional) pattern with predefined default
Default - predefined default arguments for a function

-}

-- | Pat: A pattern. Each constructor takes a [Symbol] argument, which
-- is a list of variables, each of which should be bound to the result
-- of the pattern match. We need a list because we can have something
-- like x:(y:(z:_)). If the list is empty, the pattern is unnamed.
data Pat
  = PatSymbol    ![Symbol] {-# UNPACK #-} !Symbol
  | PatLit       ![Symbol] !Literal
    -- | The second argument is an optional head constraint.
  | PatVar       ![Symbol] !(Maybe Symbol)
  | PatSeqVar    ![Symbol] !SeqType
  | PatApp       ![Symbol] !Pat !PatAppType !(Seq Pat)
  | PatAlt       ![Symbol] !Pat !Pat
  | PatOptional  ![Symbol] !Pat !Expr
  | PatCondition ![Symbol] !Pat !Expr
  deriving (Eq, Ord, Show)

data PatAppType
  = PatAppFree
  | PatAppC
  | PatAppA  !Symbol
  | PatAppAC !Symbol
  deriving (Eq, Ord, Show)

data SeqType = ZeroOrMore | OneOrMore
  deriving (Eq, Ord, Show)

instance IsString Pat where
  fromString = PatSymbol [] . fromString

-- | Map a function over the names of the outermost constructor in a
-- 'Pat'.
mapNames :: ([Symbol] -> [Symbol]) -> Pat -> Pat
mapNames f = \case
  PatSymbol    names l       -> PatSymbol    (f names) l
  PatLit       names l       -> PatLit       (f names) l
  PatVar       names h       -> PatVar       (f names) h
  PatSeqVar    names ty      -> PatSeqVar    (f names) ty
  PatApp       names h ty cs -> PatApp       (f names) h ty cs
  PatAlt       names p1 p2   -> PatAlt       (f names) p1 p2
  PatOptional  names p expr  -> PatOptional  (f names) p expr
  PatCondition names p expr  -> PatCondition (f names) p expr

-- | Get names from the outer-most constructor of a 'Pat'.
outerNames :: Pat -> [Symbol]
outerNames = \case
  PatSymbol    names _     -> names
  PatLit       names _     -> names
  PatVar       names _     -> names
  PatSeqVar    names _     -> names
  PatApp       names _ _ _ -> names
  PatAlt       names _ _   -> names
  PatOptional  names _ _   -> names
  PatCondition names _ _   -> names

addNames :: [Symbol] -> Pat -> Pat
addNames xs = mapNames (xs++)

addName :: Symbol -> Pat -> Pat
addName xs = mapNames (xs:)

pPrintNamed :: [Symbol] -> String -> String
pPrintNamed [] s = s
pPrintNamed (x:xs) s = concat
  [ pPrint x
  , ":("
  , pPrintNamed xs s
  , ")"
  ]

instance PPrint Pat where
  pPrint (PatSymbol names l) = pPrintNamed names (pPrint l)
  pPrint (PatLit    names l) = pPrintNamed names (pPrint l)
  pPrint (PatVar names h) =
    let blankStr = "_" <> maybe "" pPrint h
    in case names of
      []  -> blankStr
      [x] -> pPrint x <> blankStr
      _   -> pPrintNamed names blankStr
  pPrint (PatSeqVar names seqTy) =
    let blankStr = case seqTy of
          ZeroOrMore -> "___"
          OneOrMore  -> "__"
    in case names of
      []  -> blankStr
      [x] -> pPrint x <> blankStr
      _   -> pPrintNamed names blankStr
  pPrint (PatApp names f ty args) = pPrintNamed names $
    concat
    [ pPrint f
    , "(" ++ show ty ++ ")"
    , "["
    , intercalate ", " (map pPrint (Foldable.toList args))
    , "]"
    ]
  pPrint (PatAlt names p1 p2) = pPrintNamed names $
    pPrint p1 ++ "|" ++ pPrint p2
  pPrint (PatOptional names p e) = pPrintNamed names $
    pPrint p ++ ":" ++ pPrint e
  pPrint (PatCondition names p t) = pPrintNamed names $
    pPrint p ++ "/;" ++ pPrint t

-- | The patRootSymbol is the repeated head. If the patRootSymbol is a Symbol,
-- return it, otherwise return nothing. This function is needed for
-- automatically deducing which symbol to associate a rule to.
patRootSymbol :: Pat -> Maybe Symbol
patRootSymbol = \case
  PatSymbol _ s      -> Just s
  PatApp _ h _ _     -> patRootSymbol h
  PatCondition _ p _ -> patRootSymbol p
  _                  -> Nothing

-- | An infinite (lazy) list of symbols to be used as anonymous pattern variables
anonVars :: [Symbol]
anonVars = [ fromString ("$$PatVar" <> show i) | i <- [0 :: Int ..] ]

patFromExpr :: forall m . MonadFail m => (Symbol -> m PatAppType) -> Expr -> m Pat
patFromExpr lookupAppType = flip evalStateT 0 . go Nothing
  where
    go :: Maybe Symbol -> Expr -> StateT Int m Pat
    go currentHead expr = case expr of
      Pattern :@ Pair (ExprSymbol x) expr' ->
        addName x <$> go currentHead expr'
      Blank :@ Empty                    -> pure $ PatVar [] Nothing
      Blank :@ Solo (ExprSymbol h)      -> pure $ PatVar [] (Just h)
      BlankSequence :@ Empty            -> pure $ PatSeqVar [] OneOrMore
      BlankNullSequence :@ Empty        -> pure $ PatSeqVar [] ZeroOrMore
      Alternatives :@ pExprs@(_ :<| _) ->
        foldr1 (PatAlt []) <$> traverse (go currentHead) pExprs
      Test :@ Pair pExpr cond ->
        PatCondition [] <$> go currentHead pExpr <*> pure cond
      Optional :@ (Solo pExpr) -> do
        pat <- go currentHead pExpr
        case currentHead of
          Just h  -> pure $ PatOptional [] pat (Default :@ Solo (ExprSymbol h))
          Nothing -> fail "Optional pattern must appear under a head"
      Optional :@ Pair pExpr defExpr -> do
        pat <- go currentHead pExpr
        pure $ PatOptional [] pat defExpr
      PatternTest :@ Pair pExpr test -> do
        testVar <- newAnonVarSymbol
        pat <- go currentHead pExpr
        pure $
          PatCondition [] (addName testVar pat)
            (binary ConfirmPatternTest test (ExprSymbol testVar))
      ExprLit lit    -> pure $ PatLit [] lit
      ExprSymbol sym -> pure $ PatSymbol [] sym
      ExprApp h cs   -> do
        appType <- case h of
          ExprSymbol sym -> lift $ lookupAppType sym
          _              -> pure PatAppFree
        let headSym = case h of { ExprSymbol sym -> Just sym; _ -> Nothing }
        hPat <- go headSym h
        csPat <- traverse (go headSym) cs
        pure $ PatApp [] hPat appType csPat

    newAnonVarSymbol :: StateT Int m Symbol
    newAnonVarSymbol = do
      varNum <- state (\i -> (i, i+1))
      pure $ anonVars !! varNum
