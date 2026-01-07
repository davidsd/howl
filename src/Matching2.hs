{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NoFieldSelectors    #-}

module Matching2 where

import Data.String (IsString(..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ratio (numerator, denominator)
import Data.List (intercalate, sort)
import Control.Applicative (Alternative, empty)
import Control.Monad (guard)

class PPrint a where
  pPrint :: a -> String

newtype Symbol = MkSymbol Text
  deriving (Eq, Ord, Show)

instance PPrint Symbol where
  pPrint (MkSymbol s) = Text.unpack s

instance IsString Symbol where
  fromString = MkSymbol . fromString

data Literal
  = LitInteger Integer
  | LitRational Rational
  | LitString Text
  | LitSymbol Symbol
  deriving (Eq, Ord, Show)

showRational :: Rational -> String
showRational r =
  show (numerator r) ++
  if denominator r == 1
  then ""
  else "/" ++ show (denominator r)

instance PPrint Literal where
  pPrint (LitInteger i)  = show i
  pPrint (LitRational r) = showRational r
  pPrint (LitString s)   = show s
  pPrint (LitSymbol s)   = pPrint s

data Expr
  = ExprAtom Literal
  | ExprApp Expr [Expr]
  deriving (Eq, Ord, Show)

instance PPrint Expr where
  pPrint (ExprAtom l) = pPrint l
  pPrint (ExprApp f args) =
    mconcat
    [ pPrint f
    , "["
    , intercalate ", " (map pPrint args)
    , "]"
    ]

{- | TODO:

Composite Patterns
- p..(Repeated), p...(RepeatedNull)
- Except
- Longest
- Shortest
- OptionsPattern, PatternSequence, Verbatim, HoldPattern
- OrderlessPatternSequence
- KeyValuePattern

Restrictions on Patterns

- Condition (/;) -- Needs an evaluator! How does this work? We have to
  run the pattern match, obtain the variable bindings inside,
  substitute them into the given condition, evaluate and check if the
  result is True. We want to check the conditions greedily, so we need
  to emit them from transformMatch.

- PatternTest (?) -- Can be implemented in terms of Condition

Pattern Defaults
_:e (Optional) pattern that defaults to e if omitted
_. (Optional) pattern with predefined default
Default - predefined default arguments for a function

-}
data Pat
  = PatAtom Literal
    -- | Nothing denotes a Blank pattern. The first argument is the
    -- name of the variable, and the second is an optional head
    -- constraint.
  | PatVar (Maybe Symbol) (Maybe Symbol)
    -- | Nothing denotes a BlankSequence pattern
  | PatSeqVar SeqType (Maybe Symbol)
  | PatApp Pat [Pat]
  | PatAlt Pat Pat
  | PatNamed Symbol Pat
  | PatCondition Pat Expr
  deriving (Eq, Ord, Show)

data SeqType = ZeroOrMore | OneOrMore
  deriving (Eq, Ord, Show)

instance PPrint Pat where
  pPrint (PatAtom l) = pPrint l
  pPrint (PatVar name h) = concat
    [ maybe "" pPrint name
    , "_"
    , maybe "" pPrint h
    ]
  pPrint (PatSeqVar seqTy name) = concat
    [ maybe "" pPrint name
    , case seqTy of
        ZeroOrMore -> "___"
        OneOrMore  -> "__"
    ]
  pPrint (PatApp f args) = concat
    [ pPrint f
    , "["
    , intercalate ", " (map pPrint args)
    , "]"
    ]
  pPrint (PatAlt p1 p2) = pPrint p1 ++ "|" ++ pPrint p2
  pPrint (PatNamed s p) = pPrint s ++ ":" ++ pPrint p
  pPrint (PatCondition p t) = pPrint p ++ "/;" ++ pPrint t

{-
exprToPat :: Expr -> Maybe Pat
exprToPat (ExprApp (ExprAtom (LitSymbol "Pattern"))
           [ExprAtom (LitSymbol x), pExp) = Just (PatVar (Just x) Nothing)
exprToPat (ExprApp (ExprAtom (LitSymbol "Blank")) args) = case args of
  []                       -> Just (PatVar Nothing Nothing)
  [ExprAtom (LitSymbol h)] -> Just (PatVar Nothing (Just h))
-}

-- | For an associative head, we store the symbol inside the AppType
data AppType
  = AppFree
  | AppC
  | AppA Symbol
  | AppAC Symbol
    deriving (Eq, Ord, Show)

data Rule
  = PatRule Pat Expr
  | BuiltinRule (Expr -> Maybe Expr)

data Attribute
  = Flat
  | Orderless
  deriving (Eq, Ord, Show)

data SymbolRecord = MkSymbolRecord
  { ownValue   :: Maybe Expr
  , downValues :: [Rule] -- ^ A rule that matches expressions where
                         -- the given symbol is the head
  , upValues   :: [Rule] -- ^ A rule that matches expressions where
                         -- the given symbol is 1 level below the
                         -- head. (TODO: More cases?)
  , attributes :: Set Attribute
  }

newtype Context = MkContext (Map Symbol SymbolRecord)

emptyContext :: Context
emptyContext = MkContext Map.empty

lookupSymbol :: Symbol -> Context -> Maybe SymbolRecord
lookupSymbol s (MkContext ctx) = Map.lookup s ctx

lookupAttributes :: Symbol -> Context -> Set Attribute
lookupAttributes s ctx = case lookupSymbol s ctx of
  Nothing -> Set.empty
  Just record -> record.attributes

allRules :: Context -> [Rule]
allRules (MkContext ctx) = do
  record <- Map.elems ctx
  record.downValues <> record.upValues

-- | TODO: Change
symbolAppType :: Context -> Symbol -> AppType
symbolAppType ctx s =
  case (Set.member Flat attr, Set.member Orderless attr) of
    (False, False) -> AppFree
    (True,  False) -> AppA s
    (False, True ) -> AppC
    (True,  True ) -> AppAC s
  where
    attr = lookupAttributes s ctx

exprAppType :: Context -> Expr -> AppType
exprAppType ctx (ExprAtom (LitSymbol s)) = symbolAppType ctx s
exprAppType _   _                       = AppFree

data SolvedEq
  = SolvedVar Symbol Expr                       -- ^ x ~ t
  | SolvedSeqVar Symbol [Expr]               -- ^ \bar{x} ~ \tl{r}
  | SolvedSeqVarPerm Symbol [Expr]           -- ^ \bar{x} ~ {{ \tl r }}
  | SolvedSeqVarApp Symbol Symbol [Expr]     -- ^ \bar{x} ~ \tl t[f]
  | SolvedSeqVarAppPerm Symbol Symbol [Expr] -- ^ \bar{x} ~ {{ \tl t }}[f]
  deriving (Eq, Ord, Show)

mkSolvedVar :: Maybe Symbol -> Expr -> [SolvedEq]
mkSolvedVar Nothing _  = []
mkSolvedVar (Just x) t = [SolvedVar x t]

mkSolvedSeqVar :: Maybe Symbol -> [Expr] -> [SolvedEq]
mkSolvedSeqVar Nothing _   = []
mkSolvedSeqVar (Just x) ts = [SolvedSeqVar x ts]

mkSolvedSeqVarPerm :: Maybe Symbol -> [Expr] -> [SolvedEq]
mkSolvedSeqVarPerm Nothing _   = []
mkSolvedSeqVarPerm (Just x) ts = [SolvedSeqVarPerm x ts]

mkSolvedSeqVarApp :: Maybe Symbol -> Symbol -> [Expr] -> [SolvedEq]
mkSolvedSeqVarApp Nothing _ _   = []
mkSolvedSeqVarApp (Just x) f ts = [SolvedSeqVarApp x f ts]

mkSolvedSeqVarAppPerm :: Maybe Symbol -> Symbol -> [Expr] -> [SolvedEq]
mkSolvedSeqVarAppPerm Nothing _ _   = []
mkSolvedSeqVarAppPerm (Just x) f ts = [SolvedSeqVarAppPerm x f ts]

guardSeqTy :: Alternative f => SeqType -> [a] -> f ()
guardSeqTy OneOrMore [] = empty
guardSeqTy _         _  = pure ()

data MatchingEq
  = SingleEq Pat Expr
  | SeqEq AppType [Pat] [Expr]
  deriving (Eq, Ord, Show)

-- | Examples:
-- > split [2,3] = ([],[2,3]), ([2],[3]), ([2,3],[])
-- > split [1,2,3] = ([],[1,2,3]), ([1],[2,3]), ([1,2],[3]), ([1,2,3],[])
split :: [a] -> [([a],[a])]
split [] = [([], [])]
split (x:xs) = ([], x:xs) : do
  (xs', ys') <- split xs
  pure (x : xs', ys')

split1 :: [a] -> [([a], a, [a])]
split1 xs = do
  (l, x:r) <- split xs
  pure (l,x,r)

-- | Examples:
-- > subSequences [1,2] = [([], [1,2]), ([1],[2]), ([2],[1]), ([1,2],[])]
--
-- Note that the number of return values is exponential in the length
-- of the list. This could introduce performance issues -- hopefully
-- modified rules deal with this.
subSequences :: [a] -> [([a], [a])]
subSequences [] = [([], [])]
subSequences (x:xs) = do
  (s,rest) <- subSequences xs
  [(x:s, rest), (s, x:rest)]

data MatchStep
  = MatchBranch [SolvedEq] [MatchingEq]
  | MatchCondition [MatchingEq] Expr
  deriving (Eq, Ord, Show)

successBranch :: MatchStep
successBranch = MatchBranch [] []

-- | The _h (constrained head) pattern in Mathematica is a little
-- funny. For ExprApp's, it behaves just like h[___]. However, certain
-- h's can match literals as well. The list below is from
-- experimenting (TODO: is it documented anywhere?).
checkHead :: Maybe Symbol -> Expr -> a -> [a]
checkHead h expr x = if matchesHead h expr then [x] else []
  where
    matchesHead Nothing _ = True
    matchesHead (Just s) (ExprApp (ExprAtom (LitSymbol s')) _) = s == s'
    matchesHead (Just "Integer")  (ExprAtom (LitInteger _))  = True
    matchesHead (Just "Rational") (ExprAtom (LitRational _)) = True
    matchesHead (Just "String")   (ExprAtom (LitString _))   = True
    matchesHead (Just "Symbol")   (ExprAtom (LitSymbol _))   = True
    matchesHead _ _ = False

transformMatch :: Context -> MatchingEq -> [MatchStep]
transformMatch ctx eq = case eq of

  -- | T: Trivial
  SingleEq (PatAtom s) (ExprAtom s')
    | s == s' -> [successBranch]
    | otherwise -> []

  -- | IVE: Individual variable elimination
  SingleEq (PatVar x xHead) t -> checkHead xHead t (MatchBranch (mkSolvedVar x t) [])

  -- | A symbol cannot match with an ExprApp
  SingleEq (PatAtom _) (ExprApp _ _) -> []

  -- | A PatAlt has two branches corresponding to the two
  -- patterns. NB: This only makes sense if the same free pattern
  -- variables are present in each alternative.
  SingleEq (PatAlt p1 p2) t -> [ MatchBranch [] [SingleEq p1 t]
                               , MatchBranch [] [SingleEq p2 t]
                               ]

  -- | Named patterns
  SingleEq (PatNamed x p) t -> [MatchBranch [SolvedVar x t] [SingleEq p t]]

  -- | Conditional pattern
  SingleEq (PatCondition p test) t -> [MatchCondition [SingleEq p t] test]

  -- | A PatApp matches an ExprApp if the heads match and the sequences match
  SingleEq
    (PatApp f fArgs)
    (ExprApp g gArgs) ->
    [MatchBranch [] [SingleEq f g, SeqEq (exprAppType ctx g) fArgs gArgs]]

  -- | Free head

  -- | The empty sequence matches itself under Free
  SeqEq AppFree [] [] -> [successBranch]

  -- | SVE-F: Sequence variable elimination under Free head
  SeqEq AppFree (PatSeqVar seqTy x : ss) ts -> do
    (ts1, ts2) <- split ts
    guardSeqTy seqTy ts1
    pure $ MatchBranch (mkSolvedSeqVar x ts1) [SeqEq AppFree ss ts2]

  -- | Dec-F: Decomposition under Free head
  SeqEq AppFree (s : ss) (t : ts) ->
    [MatchBranch [] [SingleEq s t, SeqEq AppFree ss ts]]

  SeqEq AppFree _ _ -> []

  -- | The empty sequence matches itself under C
  SeqEq AppC [] [] -> [successBranch]

  -- | SVE-C: Sequence variable elimination under commutative head
  SeqEq AppC (PatSeqVar seqTy x : ss) ts -> do
    (subSeq, rest) <- subSequences ts
    guardSeqTy seqTy subSeq
    pure $ MatchBranch (mkSolvedSeqVarPerm x subSeq) [SeqEq AppC ss rest]

  -- | Dec-C: Decomposition under commutative head
  SeqEq AppC (s : ss) ts -> do
    (ts1, t, ts2) <- split1 ts
    pure $ MatchBranch [] [SingleEq s t, SeqEq AppFree ss (ts1 <> ts2)]

  SeqEq AppC _ _ -> []

  -- | Associative head
  --
  -- Note that Dec-A overlaps with FVE-A and IVE-A. It says in the paper
  -- that the form of the rules guarantees that no two rules aply to the
  -- same equation. What gives?
  --
  -- Actually on page 42, it does say there can be multiple rules that
  -- apply to a given equation, specifically it mentions Dec-A-strict
  -- and IVE-A-strict. So it seems we have to check all three rules and
  -- concatenate the results.
  
  -- | SVE-A: Sequence variable elimination under associative head
  SeqEq ty@(AppA f) (PatSeqVar seqTy x : ss) ts -> do
    (ts1, ts2) <- split ts
    guardSeqTy seqTy ts1
    pure $
      MatchBranch
      (mkSolvedSeqVarApp x f ts1)
      [SeqEq ty ss ts2]

  SeqEq ty@(AppA f) (s : ss) ts ->
    concat
    [ case ts of
        -- | Dec-A: Decomposition under associative head
        -- NB: This is the same as Dec-F
        t : ts' -> [MatchBranch [] [SingleEq s t, SeqEq ty ss ts']]
        _       -> []
    , case s of
        -- | FVE-A-strict: Function variable elimination under
        -- associative head. The strict variant imposes that s1 not be
        -- null.
        PatApp (PatVar x xHead) s1@(_:_) ->
          let xExpr = ExprAtom (LitSymbol f)
          in
            checkHead xHead xExpr $
            MatchBranch
            (mkSolvedVar x xExpr)
            [SeqEq ty (s1 <> ss) ts]
        -- | IVE-A-strict: Individual variable elimination under
        -- associative head. The strict variant imposes that t1 not be
        -- nunll.
        PatVar x xHead -> do
          (ts1@(_:_), ts2) <- split ts
          let xExpr = ExprApp (ExprAtom (LitSymbol f)) ts1
          checkHead xHead xExpr $
            MatchBranch
            (mkSolvedVar x xExpr)
            [SeqEq ty ss ts2]
        _ -> []
    ]

  -- | SVE-AC: Sequence variable elimination under AC head
  SeqEq ty@(AppAC f) (PatSeqVar seqTy x : ss) ts -> do
    (subSeq, rest) <- subSequences ts
    guardSeqTy seqTy subSeq
    pure $
      MatchBranch
      (mkSolvedSeqVarAppPerm x f subSeq)
      [SeqEq ty ss rest]

  SeqEq ty@(AppAC f) (s : ss) ts ->
    concat
    [
      -- | Dec-AC: Decomposition under AC head
      do
        (ts1, t, ts2) <- split1 ts
        pure $ MatchBranch [] [SingleEq s t, SeqEq ty ss (ts1 <> ts2)]
    ,
      case s of
        -- | FVE-AC-strict: Function variable elimination under AC
        -- head. The strict variant imposes that s1 not be null.
        PatApp (PatVar x xHead) s1@(_:_) ->
          let xExpr = ExprAtom (LitSymbol f)
          in
            checkHead xHead xExpr $
            MatchBranch
            (mkSolvedVar x xExpr)
            [SeqEq ty (s1 <> ss) ts]
        -- | IVE-AC-strict: Individual variable elimination under AC
        -- head. The strict variant imposes that subSeq not be null.
        PatVar x xHead -> do
          (subSeq@(_:_), rest) <- subSequences ts
          let xExpr = ExprApp (ExprAtom (LitSymbol f)) subSeq
          checkHead xHead xExpr $
            MatchBranch
            (mkSolvedVar x xExpr)
            [SeqEq ty ss rest]
        _ -> []
    ]

  _ -> []

checkTrue :: Context -> [SolvedEq] -> Expr -> Bool
checkTrue ctx solvedEqs expr =
  eval ctx (applySubstitutions solvedEqs expr) 
  == ExprAtom (LitSymbol "True")

solveMatch :: Context -> MatchingEq -> [[SolvedEq]]
solveMatch ctx initialMatchEq = go [initialMatchEq] []
  where
    go :: [MatchingEq] -> [SolvedEq] -> [[SolvedEq]]
    go [] solvedEqs = [solvedEqs]
    go (matchEq : matchEqs) solvedEqs = do
      transformation <- transformMatch ctx matchEq
      case transformation of
        MatchBranch newSolvedEqs newMatchEqs ->
          go (newMatchEqs ++ matchEqs) (newSolvedEqs ++ solvedEqs)
        MatchCondition newMatchEqs testExpr -> do
          solutionSet <- go (newMatchEqs ++ matchEqs) solvedEqs
          guard (checkTrue ctx solutionSet testExpr)
          pure solutionSet

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

-- TODO: Implement!
applySubstitutions :: [SolvedEq] -> Expr -> Expr
applySubstitutions _ expr = expr

tryApplyRule :: Context -> Rule -> Expr -> Maybe Expr
tryApplyRule ctx rule expr = case rule of
  PatRule pat rhs -> case solveMatch ctx (SingleEq pat expr) of
    []              -> Nothing
    (solvedEqs : _) -> Just (applySubstitutions solvedEqs rhs)
  BuiltinRule f -> f expr

flattenWithHead :: Expr -> [Expr] -> [Expr]
flattenWithHead h exprs = do
  expr <- exprs
  case expr of
    ExprApp h' args | h' == h -> flattenWithHead h args
    _                         -> pure expr

eval :: Context -> Expr -> Expr
eval ctx expr = case expr of
  ExprAtom (LitSymbol s)
    | Just record <- lookupSymbol s ctx
    , Just v      <- record.ownValue
      -- See [Note: Avoiding an infinite loop]
      -> if v == expr
         then v
         else eval ctx v
  ExprAtom _ -> expr
  ExprApp h cs ->
    let
      -- First evaluate the head and children
      h' = eval ctx h
      args' = map (eval ctx) cs

      -- If h' is a symbol with attribute Flat, then flatten h' in
      -- children. If h' is a symbol with attribute Orderless, then
      -- subsequently sort the children.
      args'' = case h' of
        ExprAtom (LitSymbol s) ->
          let
            attr = lookupAttributes s ctx
            maybeFlatten = if Set.member Flat attr      then flattenWithHead h' else id
            maybeSort    = if Set.member Orderless attr then sort               else id
          in
            maybeSort (maybeFlatten args')
        _ -> args'

      expr' = ExprApp h' args''
      
      tryRules [] = Nothing
      tryRules (rule : rules) = case tryApplyRule ctx rule expr' of
        -- Check that the transformation doesn't result in an
        -- identical expression. If it did, we'd get an infinite loop.
        result@(Just transformedExpr)
          | transformedExpr /= expr' -> result
        _ -> tryRules rules
    in
      -- Conceptually, we want to check each rule in the context to
      -- see if it matches. However, for performance reasons, we have
      -- some heuristics to determine which rules can possibly match.
      --
      -- For now, let's not worry about these heuristics and just
      -- check all rules.
      --
      -- Ideas for heuristics:
      --
      -- Downvalues: For each rule, if the total head is a symbol, we
      -- store it in the corresponding SymbolRecord as a
      -- downvalue. Thus, given an expression, we can compute its
      -- total head to lookup the corresponding rules.
      --
      -- Upvalues: If none of the Downvalues match, then we go through
      -- the children and compute their total heads. total heads of
      -- children may have associated Upvalues, and we can try these
      -- as well.
      --
      -- When computing a total head, we can record the height and
      -- associate this with the pattern and the expression, and this
      -- helps narrow down the lookup.
      case tryRules (allRules ctx) of
        Nothing -> expr'
        Just transformedExpr -> eval ctx transformedExpr

symbol :: Text -> Expr
symbol = ExprAtom . LitSymbol . MkSymbol

integer :: Integer -> Expr
integer = ExprAtom . LitInteger

rational :: Rational -> Expr
rational = ExprAtom . LitRational

patVar :: Text -> Pat
patVar name = PatVar (Just (MkSymbol name)) Nothing

blankVar :: Pat
blankVar = PatVar Nothing Nothing

myExpr :: Expr
myExpr =
  ExprApp
  (symbol "a")
  [ integer 12
  , symbol "c"
  , ExprApp
    (symbol "d")
    [ rational (17/2)
    ]
  ]

myPat :: Pat
myPat =
  PatApp
  (patVar "x")
  [ blankVar
  , PatSeqVar OneOrMore (Just "A")
  , PatSeqVar ZeroOrMore (Just "B")
  ]
