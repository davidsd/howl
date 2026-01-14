{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}

module MicroMath.Eval
  ( SubstitutionSet(..)
  , Substitution(..)
  , emptySubstitutionSet
  , insertSubstitution
  , applySubstitutions
  , solveMatch
  , eval
  ) where

import Control.Applicative (Alternative, empty)
import Control.Monad       (foldM, guard)
import Data.Foldable       qualified as Foldable
import Data.Map.Strict     (Map)
import Data.Map.Strict     qualified as Map
import Data.Maybe          (mapMaybe)
import Data.Sequence       (Seq, pattern (:<|), pattern Empty)
import Data.Sequence       qualified as Seq
import Debug.Trace         qualified as Debug
import MicroMath.Context   (Attributes (..), Context (..), HoldType (..),
                            Rule (..), SymbolRecord (..), lookupAttributes,
                            lookupSymbolRecord)
import MicroMath.Expr      (Expr (..), Literal (..), flattenWithHead,
                            mapSymbols)
import MicroMath.Expr      qualified as Expr
import MicroMath.Pat       (Pat (..), SeqType (..), addNames)
import MicroMath.Symbol    (Symbol)
import MicroMath.Util      (splits, splits1, subSequences)

{- ============== Pattern Matching ================

The pattern matchign algorithm implemented here is described in
https://www.sciencedirect.com/science/article/pii/S0747717121000079
(pdf here:
https://www3.risc.jku.at/publications/download/risc_6260/variadic-equational-matching-jsc-final-with-mma-versions.pdf)

Specifically, we implement algorithm M_{Mma}, which is an incomplete,
strict variant of the algorithm described in that paper.

-}

data Marking
  = Mark0
  | Mark1
  deriving (Eq, Ord, Show)

-- | For an associative head, we store the symbol inside the AppType
data AppType
  = AppFree
  | AppC
  | AppA Symbol (Maybe Marking)
  | AppAC Symbol (Maybe Marking)
    deriving (Eq, Ord, Show)

symbolAppType :: Context -> Symbol -> AppType
symbolAppType ctx s =
  case (attr.flat, attr.orderless) of
    (False, False) -> AppFree
    (True,  False) -> AppA s Nothing
    (False, True ) -> AppC
    (True,  True ) -> AppAC s Nothing
  where
    attr = lookupAttributes s ctx

exprAppType :: Context -> Expr -> AppType
exprAppType ctx (ExprSymbol s) = symbolAppType ctx s
exprAppType _   _              = AppFree

data Substitution = MkSubstitution Symbol Expr
  deriving (Eq, Ord, Show)

-- | A set of substitutions, with at most one substitution for each
-- symbol.
newtype SubstitutionSet = MkSubstitutionSet (Map Symbol Expr)
  deriving (Eq, Ord, Show)

emptySubstitutionSet :: SubstitutionSet
emptySubstitutionSet = MkSubstitutionSet Map.empty

-- | Attempt to insert a Substitution. If an old binding is already
-- present and agrees with the new one, or if there was no old
-- binding, then return the result. Otherwise there is a conflict --
-- return Nothing.
insertSubstitution :: Substitution -> SubstitutionSet -> Maybe SubstitutionSet
insertSubstitution (MkSubstitution s b) (MkSubstitutionSet m) =
  case insertLookup s b m of
    (Just b', result) | b == b' -> Just (MkSubstitutionSet result)
    (Nothing, result)           -> Just (MkSubstitutionSet result)
    _                           -> Nothing
  where
    insertLookup kx x t = Map.insertLookupWithKey (\_ a _ -> a) kx x t

-- | Attempt to insert the given substitutions.
insertSubstitutions :: [Substitution] -> SubstitutionSet -> Maybe SubstitutionSet
insertSubstitutions subs set =
  foldM (flip insertSubstitution) set subs

-- | Lookup the Binding corresponding to a Symbol in the given
-- SubstitutionSet
lookupBinding :: Symbol -> SubstitutionSet -> Maybe Expr
lookupBinding s (MkSubstitutionSet m) = Map.lookup s m

-- | Replace the Symbols in the given Expr with their corresponding
-- Bindings in 'substSet'.
applySubstitutions :: SubstitutionSet -> Expr -> Expr
applySubstitutions substSet = mapSymbols $ \s ->
  case lookupBinding s substSet of
    Nothing -> ExprSymbol s
    Just b  -> b

bindVars :: [Symbol] -> Expr -> [Substitution]
bindVars xs t = [MkSubstitution x t | x <- xs]

bindSeqVars :: [Symbol] -> Seq Expr -> [Substitution]
bindSeqVars xs ts = [MkSubstitution x (ExprApp Expr.Sequence ts) | x <- xs]

guardSeqTy :: Alternative f => SeqType -> Seq a -> f ()
guardSeqTy OneOrMore Empty = empty
guardSeqTy _         _     = pure ()

data MatchingEq
  = SingleEq Pat Expr
  | SeqEq AppType (Seq Pat) (Seq Expr)
  deriving (Eq, Ord, Show)

data MatchStep
  = MatchBranch [Substitution] [MatchingEq]
  | MatchCondition [MatchingEq] Expr
  deriving (Eq, Ord, Show)

-- | The _h (constrained head) pattern in Mathematica is a little
-- funny. For ExprApp's, it behaves just like h[___]. However, certain
-- h's can match literals as well. The list below is from
-- experimenting (TODO: is it documented anywhere?).
checkHead :: Maybe Symbol -> Expr -> a -> [a]
checkHead h expr x = if matchesHead h expr then [x] else []
  where
    matchesHead Nothing _                                   = True
    matchesHead (Just s) (ExprApp (ExprSymbol s') _)        = s == s'
    matchesHead (Just "Symbol")   (ExprSymbol _)            = True
    matchesHead (Just "Integer")  (ExprLit (LitInteger _))  = True
    matchesHead (Just "Rational") (ExprLit (LitRational _)) = True
    matchesHead (Just "Real")     (ExprLit (LitReal _))     = True
    matchesHead (Just "String")   (ExprLit (LitString _))   = True
    matchesHead _ _                                         = False

transformMatch :: Context -> MatchingEq -> [MatchStep]
transformMatch ctx eq = case eq of

  -- | T: Trivial
  SingleEq (PatLit xs s) expr@(ExprLit s')
    | s == s' -> [MatchBranch (bindVars xs expr) []]
    | otherwise -> []

  SingleEq (PatSymbol xs s) expr@(ExprSymbol s')
    | s == s' -> [MatchBranch (bindVars xs expr) []]
    | otherwise -> []

  -- | IVE: Individual variable elimination
  SingleEq (PatVar x xHead) t -> checkHead xHead t (MatchBranch (bindVars x t) [])

  -- | A symbol or literal cannot match with an ExprApp
  SingleEq (PatSymbol _ _) (ExprApp _ _) -> []
  SingleEq (PatLit _ _)    (ExprApp _ _) -> []

  -- | A PatAlt has two branches corresponding to the two
  -- patterns. NB: This only makes sense if the same free pattern
  -- variables are present in each alternative.
  SingleEq (PatAlt xs p1 p2) t ->
    [ MatchBranch [] [SingleEq (addNames xs p1) t]
    , MatchBranch [] [SingleEq (addNames xs p2) t]
    ]

  -- | Conditional pattern
  SingleEq (PatCondition xs p test) t ->
    [ MatchCondition [SingleEq (addNames xs p) t] test ]

  -- | A PatApp matches an ExprApp if the heads match and the sequences match
  SingleEq
    (PatApp xs f fArgs)
    expr@(ExprApp g gArgs) ->
    -- | FVE-M: Function variable elimination (Mathematica). If we
    -- eliminate a function variable, then the head becomes Free,
    -- regardless of the symbol it is bound to. This is a little
    -- weird, but it is how Mathematica works.
    let appTy = case f of
          PatVar _ _ -> AppFree
          _          -> exprAppType ctx g
    in
      [MatchBranch (bindVars xs expr) [SingleEq f g, SeqEq appTy fArgs gArgs]]

  -- | Free head

  -- | The empty sequence matches itself under any head.
  SeqEq _ Empty Empty -> [MatchBranch [] []]

  -- | SVE: Sequence variable elimination (applies under any head)
  SeqEq appTy (PatSeqVar x seqTy :<| ss) ts -> do
    (ts1, ts2) <- splits ts
    guardSeqTy seqTy ts1
    pure $ MatchBranch (bindSeqVars x ts1) [SeqEq appTy ss ts2]

  -- | Dec-F: Decomposition under Free head
  SeqEq AppFree (s :<| ss) (t :<| ts) ->
    [MatchBranch [] [SingleEq s t, SeqEq AppFree ss ts]]

  -- | Commutative head
  --
  -- | SVE-C: Sequence variable elimination under commutative
  -- head. Dropped in Mathematica -- use SVE instead.
  --
  -- | Dec-C: Decomposition under commutative head
  SeqEq AppC (s :<| ss) ts -> do
    (ts1, t, ts2) <- splits1 ts
    pure $ MatchBranch [] [SingleEq s t, SeqEq AppFree ss (ts1 <> ts2)]

  SeqEq AppC _ _ -> []

  -- | Associative head
  --
  -- | SVE-A: Sequence variable elimination under associative
  -- head. Dropped in Mathematica -- use SVE insead.
  --
  -- | FVE-A-strict: Function variable elimination under associative
  -- head. This rule is dropped in Mathematica. It is also practically
  -- difficult to formulate this rule in the case where the function
  -- variable application is named: y:(x_[...]).
  --
  SeqEq (AppA f marking) (s :<| ss) ts ->
    concat
    [ case ts of
        -- | Dec-A: Decomposition under associative head
        -- TODO: What is the strict version of this rule? Is it
        -- already strict?
        --
        -- Markings:
        --
        -- - If unmarked, mark by 0
        -- - If marked by 1, then Dec-A-strict does not apply
        -- - If marked by 0, then retain marking 0
        --
        t :<| ts'
          | marking /= Just Mark1
            ->
            let
              newMarking = case marking of
                Nothing -> Just Mark0
                _       -> marking
              newTy = AppA f newMarking
            in
              [MatchBranch [] [SingleEq s t, SeqEq newTy ss ts']]
        _   -> []
    , case s of
        -- | IVE-A-strict: Individual variable elimination under
        -- associative head. The strict variant imposes that t1 not be
        -- null.
        --
        -- Markings:
        --
        -- - IVE-A-strict does not apply if mark=0 and length ts1<=1.
        -- - If unmarked and ts1 has length 1, mark by 1
        -- - otherwise retain the marking
        PatVar x xHead -> do
          (ts1@(_:<|_), ts2) <- splits ts
          guard $ not $ marking == Just Mark0 && length ts1 <= 1
          let
            xExpr = ExprApp (ExprSymbol f) ts1
            newMarking = case marking of
              Nothing | length ts1 <= 1 -> Just Mark1
              _                         -> marking
            newTy = AppA f newMarking
          checkHead xHead xExpr $
            MatchBranch
            (bindVars x xExpr)
            [SeqEq newTy ss ts2]
        _ -> []
    ]

  -- | Associative-Commutative head
  --
  -- | SVE-AC: Sequence variable elimination under AC head. Dropped in
  -- Mathematica -- use SVE instead.
  --
  -- | FVE-AC-strict: Function variable elimination under AC
  -- head. This rule is absent in Mathematica's matching algorithm.
  --
  -- TODO: These rules are very similar to the AppA rules, with the
  -- only differences being uncons vs splits1 for Dec-A vs Dec-AC, and
  -- splits vs subSequences for IVE-A-strict vs IVE-AC-strict. We could
  -- try to deduplicate the code.
  --
  SeqEq (AppAC f marking) (s :<| ss) ts ->
    concat
    [
      -- | Dec-AC: Decomposition under AC head. Marking rules the same as AppA.
      -- TODO: Is this already strict?
      case marking of
        Just Mark1 -> []
        _ -> do
          (ts1, t, ts2) <- splits1 ts
          let
            newMarking = case marking of
              Nothing -> Just Mark0
              _       -> marking
            newTy = AppAC f newMarking
          pure $ MatchBranch [] [SingleEq s t, SeqEq newTy ss (ts1 <> ts2)]
    ,
      case s of
        -- | IVE-AC-strict: Individual variable elimination under AC
        -- head. The strict variant imposes that subSeq not be null.
        PatVar x xHead -> do
          (subSeq@(_:<|_), rest) <- subSequences ts
          guard $ not $ marking == Just Mark0 && length subSeq <= 1
          let
            xExpr = ExprApp (ExprSymbol f) subSeq
            newMarking = case marking of
              Nothing | length subSeq <= 1 -> Just Mark1
              _                            -> marking
            newTy = AppAC f newMarking
          checkHead xHead xExpr $
            MatchBranch
            (bindVars x xExpr)
            [SeqEq newTy ss rest]
        _ -> []
    ]

  _ -> []

checkTrue :: Context -> Expr -> Bool
checkTrue ctx expr = eval ctx expr == Expr.True

solveMatch :: Context -> MatchingEq -> [SubstitutionSet]
solveMatch ctx initialMatchEq = go [initialMatchEq] emptySubstitutionSet
  where
    go :: [MatchingEq] -> SubstitutionSet -> [SubstitutionSet]
    go [] substSet = [substSet]
    go (matchEq : matchEqs) substSet = do
      transformation <- transformMatch ctx matchEq
      case transformation of
        MatchBranch newSubstitutions newMatchEqs ->
          case insertSubstitutions newSubstitutions substSet of
            Just newSubstSet -> go (newMatchEqs ++ matchEqs) newSubstSet
            Nothing          -> []
        MatchCondition newMatchEqs testExpr -> do
          solutionSet <- go (newMatchEqs ++ matchEqs) substSet
          guard $ checkTrue ctx (applySubstitutions solutionSet testExpr)
          pure solutionSet

{- ============== Evaluation ================

Mathematica's standard evaluation sequence is described here
(https://reference.wolfram.com/language/tutorial/Evaluation.html). Unfortunately,
it contains some typos, which I'll try to fix below. I also added
implementation comments for the current status in MicroMath.

- If the expression is a raw object (e.g., Integer, String, etc.),
  leave it unchanged. MicroMath: Done.

- Evaluate the head h of the expression. MicroMath: Done.

- Evaluate each element ei of the expression in turn. If h is a symbol
  with attributes HoldFirst, HoldRest, HoldAll, or HoldAllComplete,
  then skip evaluation of certain elements. MicroMath: HoldAllComplete
  not implemented.

- Unless h has attributes SequenceHold or HoldAllComplete, flatten out
  all Sequence objects that appear among the ei. MicroMath:
  SequenceHold/HoldAllComplete not implemented.

- Unless h has attribute HoldAllComplete, strip the outermost of any
  Unevaluated wrappers that appear among the ei. MicroMath: Not
  implemented.

- If h has attribute Flat, then flatten out all nested expressions
  with head h. MicroMath: Done.

- If h has attribute Listable, then thread through any ei that are
  lists. MicroMath: Listable not implemented.

- If h has attribute Orderless, then sort the ei into
  order. MicroMath: Done.

- Unless h has attribute HoldAllComplete, use any user-defined
  upvalues of symbols f appearing in the form h[f[e1,...],...]. NB: We
  do not look for upvalues inside the head. So for exampe, we cannot
  define an upvalue of the following form:

  g /: f[g[x_]][1] := foo; (BAD)

  Because g appears inside the head of the expression and not in the
  sequence of arguments, it is not considered to be at "level 1".

- Use any builtin upvalues. MicroMath: We allow the user to choose the
  ordering, and there is no special privilege given to
  builtin/user-defined UpValues.

- Use any user-defined down-values associated to h in the form
  h[...] or for h[...][...]. Presumably this also means repeated
  curried application h[...][...][...]. MicroMath: Mathematica
  distinguishes between down-values and sub-values. For now, we don't
  distinguish --- down values are associated to the root symbol.

- Use any built‐in transformation rules for h[...] or
  h[...][...]. MicroMath: Again, we don't distinguish between
  built-in/user-defined downvalues.

Note that, conceptually, we would like to try every rule in the
Context when evaluating expressions. However, if the Context is large,
this would have poor performance. This business of up-values and
down-values is essentially a set of heuristics to pre-sort rules in
such a way that we know whether they have a chance of matching or not.

-}

{- [Note: Avoiding an infinite loop]

Note that Mathematica avoids infinite loops when a symbol is identically
equal to itself:

FOO := FOO
In[1] := FOO
Out[1] := FOO

However, it does loop if the symbol simply evaluates to itself:

BAR := BAR + 1 - 1;
In[1] := BAR
Out[1] := TerminatedEvaluation[RecursionLimit]

-}

-- | Try to apply a rule to the given expression. If the rule matches,
-- return 'Just result' otherwise return nothing. If there are
-- multiple matches, we pick the first one. Note that since solveMatch
-- returns lists, which are computed lazily, this means we terminate
-- early as soon as a match is found.
tryApplyRule :: Context -> Rule -> Expr -> Maybe Expr
tryApplyRule ctx rule expr = case rule of
  PatRule pat rhs -> case solveMatch ctx (SingleEq pat expr) of
    []             -> Nothing
    (substSet : _) -> Just (applySubstitutions substSet rhs)
  BuiltinRule f -> f ctx expr

mapWithHoldType :: Maybe HoldType -> (a -> a) -> Seq a -> Seq a
mapWithHoldType Nothing          f xs         = fmap f xs
mapWithHoldType _                _ Empty      = Empty
mapWithHoldType (Just HoldAll)   _ xs         = xs
mapWithHoldType (Just HoldFirst) f (x :<| xs) = x :<| fmap f xs
mapWithHoldType (Just HoldRest)  f (x :<| xs) = f x :<| xs

-- | Flatten any occurrences of Sequence[...] in the given list of
-- expressions. Note that this works with nested Sequence as well,
-- e.g.  flattenSequences [Sequence[Sequence[a]],b] -> [a,b]
flattenSequences :: Seq Expr -> Seq Expr
flattenSequences = flattenWithHead Expr.Sequence

-- | Extract the symbol g from expressions that are pure g or of the form g[...]
level0Symbol :: Expr -> Maybe Symbol
level0Symbol = \case
  ExprSymbol s             -> Just s
  ExprApp (ExprSymbol s) _ -> Just s
  _                        -> Nothing

eval :: Context -> Expr -> Expr
eval ctx expr = case expr of
  ExprSymbol s
    | Just record <- lookupSymbolRecord s ctx
    , Just v      <- record.ownValue
      -- See [Note: Avoiding an infinite loop]
      -> if v == expr
         then v
         else eval ctx v
    | otherwise -> expr
  ExprLit _ -> expr
  ExprApp h cs ->
    let
      -- Evaluate the head and children, and flatten any sequences
      -- appearing in the children.
      --
      -- TODO: if h' has attributes SequenceHold or HoldAllComplete,
      -- then don't flatten Sequence's.
      h' = eval ctx h

      -- If h' is a symbol with attribute Flat, then flatten h' in
      -- children. If h' is a symbol with attribute Orderless, then
      -- subsequently sort the children.
      cs' = case h' of
        ExprSymbol s ->
          let
            attr = lookupAttributes s ctx
            maybeFlatten  = if attr.flat      then flattenWithHead h' else id
            maybeSort     = if attr.orderless then Seq.unstableSort   else id
            maybeEvalArgs = mapWithHoldType attr.holdType (eval ctx)
          in
            maybeSort . maybeFlatten . flattenSequences . maybeEvalArgs $ cs
        _ -> flattenSequences $ fmap (eval ctx) cs

      expr' = ExprApp h' cs'

      downRules
        | Just rootSym <- Expr.rootSymbol h'
        , Just record  <- lookupSymbolRecord rootSym ctx
        = Foldable.toList record.downValues
        | otherwise = []

      level1Symbols = mapMaybe level0Symbol (Foldable.toList cs')
      upRules = do
        sym <- level1Symbols
        case lookupSymbolRecord sym ctx of
          Just record -> Foldable.toList record.upValues
          Nothing     -> []

      tryRules [] = Nothing
      tryRules (rule : rules) = case tryApplyRule ctx rule expr' of
        -- Check that the transformation doesn't result in an
        -- identical expression. If it does, we'd get an infinite loop
        -- if we evaluated again.
        result@(Just transformedExpr)
          | transformedExpr /= expr' -> Debug.traceShow (rule, expr', transformedExpr) result
        _ -> tryRules rules

    in
      -- Try upRules before downRules
      case tryRules (upRules <> downRules) of
        Nothing              -> expr'
        Just transformedExpr -> eval ctx transformedExpr
