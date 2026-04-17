{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoFieldSelectors  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

-- | Internal pattern representation used by the evaluator.
--
-- These patterns are compiled from Wolfram Language pattern expressions
-- and then used during matching.
module Howl.Pat
  ( Pat(..)
  , PatAppType(..)
  , SeqType(..)
  , outerNames
  , patNames
  , mapNames
  , addNames
  , matchesUniqueExpr
  , patRootSymbol
  , patFromExpr
  ) where

import Control.Monad.State (StateT, evalStateT, lift, state)
import Data.Foldable       qualified as Foldable
import Data.List           (intercalate)
import Data.Sequence       (Seq, pattern (:<|), pattern Empty)
import Data.Set            (Set)
import Data.Set            qualified as Set
import Data.String         (IsString (..))
import Howl.Expr           (Expr (..), Literal, binary, pattern (:@),
                            pattern Alternatives, pattern Blank,
                            pattern BlankNullSequence, pattern BlankSequence,
                            pattern ConfirmPatternTest, pattern Default,
                            pattern Optional, pattern Pattern,
                            pattern PatternTest, pattern Test)
import Howl.Expr.PPrint    ()
import Howl.PPrint         (PPrint (..))
import Howl.Symbol         (Symbol)
import Howl.Util           (pattern Pair, pattern Solo)

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
_. (Optional) pattern with predefined default
Default - predefined default arguments for a function

-}

-- | A compiled pattern.
--
-- Each constructor takes a list of variable names to bind to the
-- result of the match. We need a list because we can have nested
-- named patterns such as @x:(y:(z:_))@. If the list is empty, the
-- pattern is unnamed.
data Pat
  -- | A literal symbol pattern.
  = PatSymbol
      { names  :: ![Symbol]
      , symbol :: {-# UNPACK #-} !Symbol
      }
  -- | A literal (numeric/string) pattern.
  | PatLit
      { names   :: ![Symbol]
      , literal :: !Literal
      }
  -- | A blank pattern, optionally constrained by a head symbol.
  | PatVar
      { names          :: ![Symbol]
      , headConstraint :: !(Maybe Symbol)
      }
  -- | A sequence blank (@__@ or @___@).
  | PatSeqVar
      { names   :: ![Symbol]
      , seqType :: !SeqType
      }
  -- | A head application pattern with precomputed app attributes.
  | PatApp
      { names   :: ![Symbol]
      , headPat :: !Pat
      , appType :: !PatAppType
      , args    :: !(Seq Pat)
      }
  -- | Alternatives, with precomputed missing names for each branch.
  --   When the left branch matches we bind missingInLeft to
  --   'Sequence[]', and symmetrically for the right branch.
  | PatAlt
      { names          :: ![Symbol]
      , missingInLeft  :: ![Symbol]
      , missingInRight :: ![Symbol]
      , left           :: !Pat
      , right          :: !Pat
      }
  -- | An optional pattern with a default expression.
  | PatOptional
      { names       :: ![Symbol]
      , pat         :: !Pat
      , defaultExpr :: !Expr
      }
  -- | A pattern with a /; condition.
  | PatCondition
      { names    :: ![Symbol]
      , pat      :: !Pat
      , testExpr :: !Expr
      }
  deriving (Eq, Ord, Show)

-- | Application-type information attached to a compiled head pattern.
--
-- This records whether the matched head should be treated as free,
-- commutative, associative, or associative-commutative.
data PatAppType
  = PatAppFree
  | PatAppC
  | PatAppA  !Symbol
  | PatAppAC !Symbol
  deriving (Eq, Ord, Show)

-- | The multiplicity of a sequence pattern.
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
  PatAlt       names m1 m2 p1 p2 -> PatAlt   (f names) m1 m2 p1 p2
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
  PatAlt       names _ _ _ _ -> names
  PatOptional  names _ _   -> names
  PatCondition names _ _   -> names

-- | All pattern variable names mentioned anywhere in a pattern.
patNames :: Pat -> Set Symbol
patNames = \case
  PatSymbol names _     -> Set.fromList names
  PatLit names _        -> Set.fromList names
  PatVar names _        -> Set.fromList names
  PatSeqVar names _     -> Set.fromList names
  PatApp names h _ args ->
    Set.unions [Set.fromList names, patNames h, Foldable.foldMap patNames args]
  PatAlt names _ _ p1 p2 ->
    Set.unions [Set.fromList names, patNames p1, patNames p2]
  PatOptional names p _ ->
    Set.union (Set.fromList names) (patNames p)
  PatCondition names p _ ->
    Set.union (Set.fromList names) (patNames p)

-- | Add the given names to the outermost constructor of a pattern.
addNames :: [Symbol] -> Pat -> Pat
addNames xs = mapNames (xs++)

addName :: Symbol -> Pat -> Pat
addName xs = mapNames (xs:)

-- | Check whether a pattern matches a unique expression and introduces
-- no bindings.
--
-- For example, @Foo[12]@ matches a unique expression, but @Foo[x_]@
-- does not. This function is conservative and only uses syntax
-- information, so there may be cases where the pattern does match a
-- unique expression but this function does not detect it. For example,
-- @(x_/;SameQ[x,12])@ matches only @12@, but this function returns
-- 'Nothing'.
--
-- It is also important to reject patterns that introduce bindings
-- because we are short-circuiting the pattern matching process, and so
-- those bindings would not happen. TODO: we could pre-compute the
-- bindings and store them.
matchesUniqueExpr :: Pat -> Maybe Expr
matchesUniqueExpr pat
  | not (Set.null (patNames pat)) = Nothing
  | otherwise = case pat of
      PatSymbol _ sym -> Just (ExprSymbol sym)
      PatLit _ lit -> Just (ExprLit lit)
      PatVar {} -> Nothing
      PatSeqVar {} -> Nothing
      PatApp _ h _ args -> do
        h' <- matchesUniqueExpr h
        args' <- traverse matchesUniqueExpr args
        pure $ ExprApp h' args'
      PatAlt _ _ _ p1 p2 -> do
        e1 <- matchesUniqueExpr p1
        e2 <- matchesUniqueExpr p2
        if e1 == e2 then Just e1 else Nothing
      PatOptional {} -> Nothing
      PatCondition {} -> Nothing

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
  pPrint (PatAlt names _ _ p1 p2) = pPrintNamed names $
    pPrint p1 ++ "|" ++ pPrint p2
  pPrint (PatOptional names p e) = pPrintNamed names $
    pPrint p ++ ":" ++ pPrint e
  pPrint (PatCondition names p t) = pPrintNamed names $
    pPrint p ++ "/;" ++ pPrint t

-- | Return the repeated head symbol of a pattern, if there is one.
--
-- This is used to deduce which symbol a rule should be associated
-- with.
patRootSymbol :: Pat -> Maybe Symbol
patRootSymbol = \case
  PatSymbol _ s      -> Just s
  PatApp _ h _ _     -> patRootSymbol h
  PatCondition _ p _ -> patRootSymbol p
  _                  -> Nothing

-- | An infinite lazy list of symbols used as anonymous pattern variables.
anonVars :: [Symbol]
anonVars = [ fromString ("$$PatVar" <> show i) | i <- [0 :: Int ..] ]

-- | Compile a Wolfram Language pattern expression into a 'Pat'.
--
-- The callback is used to determine the application type of head
-- symbols while compiling application patterns.
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
        foldr1 mkPatAlt <$> traverse (go currentHead) pExprs
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

    mkPatAlt :: Pat -> Pat -> Pat
    mkPatAlt p1 p2 =
      let
        n1 = patNames p1
        n2 = patNames p2
        miss1 = Set.toList (Set.difference n2 n1)
        miss2 = Set.toList (Set.difference n1 n2)
      in PatAlt [] miss1 miss2 p1 p2
