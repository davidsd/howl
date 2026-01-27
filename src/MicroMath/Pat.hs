{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module MicroMath.Pat
  ( Pat(..)
  , PatAppType(..)
  , SeqType(..)
  , mapNames
  , addNames
  , patRootSymbol
  , patFromExpr
  ) where

import Control.Monad.State   (StateT, evalStateT, lift, state)
import Data.Foldable         qualified as Foldable
import Data.List             (intercalate)
import Data.Sequence         (Seq, pattern (:<|), pattern (:|>), pattern Empty)
import Data.Set              (Set)
import Data.Set              qualified as Set
import Data.String           (IsString (..))
import MicroMath.Expr        (Expr (..), Literal, binary, pattern Alternatives,
                              pattern Blank, pattern BlankNullSequence,
                              pattern BlankSequence, pattern ConfirmPatternTest,
                              pattern Pattern, pattern PatternTest,
                              pattern Test)
import MicroMath.Expr.PPrint ()
import MicroMath.PPrint      (PPrint (..))
import MicroMath.Symbol      (Symbol)

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
  | PatCondition ![Symbol] !(Set Symbol) !Pat !Expr
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

mapNames :: ([Symbol] -> [Symbol]) -> Pat -> Pat
mapNames f = \case
  PatSymbol    names l      -> PatSymbol    (f names) l
  PatLit       names l      -> PatLit       (f names) l
  PatVar       names h      -> PatVar       (f names) h
  PatSeqVar    names ty     -> PatSeqVar    (f names) ty
  PatApp       names h ty cs -> PatApp      (f names) h ty cs
  PatAlt       names p1 p2  -> PatAlt       (f names) p1 p2
  PatCondition names _ p expr ->
    let names' = f names
    in PatCondition names' (patNamesPlain names' p) p expr

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
  pPrint (PatCondition names _ p t) = pPrintNamed names $
    pPrint p ++ "/;" ++ pPrint t

-- | The patRootSymbol is the repeated head. If the patRootSymbol is a Symbol,
-- return it, otherwise return nothing. This function is needed for
-- automatically deducing which symbol to associate a rule to.
patRootSymbol :: Pat -> Maybe Symbol
patRootSymbol = \case
  PatSymbol _ s      -> Just s
  PatApp _ h _ _     -> patRootSymbol h
  PatCondition _ _ p _ -> patRootSymbol p
  _                  -> Nothing

-- | An infinite (lazy) list of symbols to be used as anonymous pattern variables
anonVars :: [Symbol]
anonVars = [ fromString ("$$PatVar" <> show i) | i <- [0 :: Int ..] ]

patFromExpr :: forall m . Monad m => (Symbol -> m PatAppType) -> Expr -> m Pat
patFromExpr lookupAppType = flip evalStateT 0 . go
  where
    go :: Expr -> StateT Int m Pat
    go expr = case expr of
      ExprApp Pattern (ExprSymbol x :<| expr' :<| Empty) ->
        addName x <$> go expr'
      ExprApp Blank Empty                    -> pure $ PatVar [] Nothing
      ExprApp Blank (ExprSymbol h :<| Empty) -> pure $ PatVar [] (Just h)
      ExprApp BlankSequence Empty            -> pure $ PatSeqVar [] OneOrMore
      ExprApp BlankNullSequence Empty        -> pure $ PatSeqVar [] ZeroOrMore
      ExprApp Alternatives pExprs@(_ :<| _) ->
        foldr1 (PatAlt []) <$> traverse go pExprs
      ExprApp Test (pExpr :<| cond :<| Empty) ->
        mkPatCondition [] <$> go pExpr <*> pure cond
      ExprApp PatternTest (pExpr :<| test :<| Empty) -> do
        testVar <- newAnonVarSymbol
        pat <- go pExpr
        pure $
          mkPatCondition [] (addName testVar pat)
            (binary ConfirmPatternTest test (ExprSymbol testVar))
      ExprLit lit    -> pure $ PatLit [] lit
      ExprSymbol sym -> pure $ PatSymbol [] sym
      ExprApp h cs   -> do
        appType <- case h of
          ExprSymbol sym -> lift $ lookupAppType sym
          _              -> pure PatAppFree
        hPat <- go h
        (csPat, mkConditions) <- floatOutConditions <$> traverse go cs
        pure $ mkConditions $ PatApp [] hPat appType csPat

    newAnonVarSymbol :: StateT Int m Symbol
    newAnonVarSymbol = do
      varNum <- state (\i -> (i, i+1))
      pure $ anonVars !! varNum

-- | Given a sequence of Pat's, float any PatCondition's on elements
-- of the sequence out to the top level by collecting them in a
-- mkConditions function. This is needed because the pattern matcher
-- only recognizes PatCondition's on SingleEq's. This is probably best
-- because 'SeqEq's need to be fast in order to search over many
-- possibilities. TODO: Is this true?
floatOutConditions :: Seq Pat -> (Seq Pat, Pat -> Pat)
floatOutConditions = goFloat Empty id
  where
    goFloat pats mkConditions Empty = (pats, mkConditions)
    goFloat pats mkConditions (PatCondition names _ pat test :<| rest) =
      goFloat pats ((\p -> mkPatCondition names p test) . mkConditions) (pat :<| rest)
    goFloat pats mkConditions (pat :<| rest) = goFloat (pats :|> pat) mkConditions rest

patNamesPlain :: [Symbol] -> Pat -> Set Symbol
patNamesPlain names p =
  Set.union (Set.fromList names) (patNamesInner p)
  where
    patNamesInner = \case
      PatSymbol ns _     -> Set.fromList ns
      PatLit ns _        -> Set.fromList ns
      PatVar ns _        -> Set.fromList ns
      PatSeqVar ns _     -> Set.fromList ns
      PatApp ns h _ args ->
        Set.unions [Set.fromList ns, patNamesInner h, Foldable.foldMap patNamesInner args]
      PatAlt ns p1 p2    ->
        Set.unions [Set.fromList ns, patNamesInner p1, patNamesInner p2]
      PatCondition ns _ p' _ ->
        Set.union (Set.fromList ns) (patNamesInner p')

mkPatCondition :: [Symbol] -> Pat -> Expr -> Pat
mkPatCondition names p expr =
  PatCondition names (patNamesPlain names p) p expr
