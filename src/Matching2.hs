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
import Control.Monad (guard, foldM)

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

instance IsString Literal where
  fromString = LitSymbol . fromString

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

instance IsString Expr where
  fromString = ExprAtom . fromString

unary :: Expr -> Expr -> Expr
unary e x = e![x]

binary :: Expr -> Expr -> Expr -> Expr
binary e x y = e![x,y]

instance Num Expr where
  (+) = binary "Plus"
  (*) = binary "Times"
  negate x = (-1)*x
  abs = unary "Abs"
  signum = unary "Signum"
  fromInteger = ExprAtom . LitInteger

instance Fractional Expr where
  recip x = "Power"![x,-1]
  fromRational = ExprAtom . LitRational

instance Floating Expr where
  pi = "Pi"
  exp = unary "Exp"
  log = unary "Log"
  sqrt x = "Power"![x, fromRational (1/2)]
  logBase = binary "Log"
  (**) = binary "Power"
  sin = unary "Sin"
  cos = unary "Cos"
  tan = unary "Tan"
  asin = unary "ArcSin"
  acos = unary "ArcCos"
  atan = unary "ArcTan"
  sinh = unary "Sinh"
  cosh = unary "Cosh"
  tanh = unary "Tanh"
  asinh = unary "ArcSinh"
  acosh = unary "ArcCosh"
  atanh = unary "ArcTanh"

instance PPrint Expr where
  pPrint (ExprAtom l) = pPrint l
  pPrint (ExprApp f args) =
    mconcat
    [ pPrint f
    , "["
    , intercalate ", " (map pPrint args)
    , "]"
    ]

-- | Map a function over the symbols present in an expression
mapSymbols :: (Symbol -> Expr) -> Expr -> Expr
mapSymbols f expr = case expr of
  ExprAtom (LitSymbol s) -> f s
  ExprAtom _             -> expr
  ExprApp h cs           -> ExprApp (mapSymbols f h) (map (mapSymbols f) cs)

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

- PatternTest (?) -- Can be implemented in terms of Condition

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
  = PatAtom [Symbol] Literal
    -- | The second argument is an optional head constraint.
  | PatVar [Symbol] (Maybe Symbol)
  | PatSeqVar [Symbol] SeqType
  | PatApp [Symbol] Pat [Pat]
  | PatAlt [Symbol] Pat Pat
  | PatCondition [Symbol] Pat Expr
  deriving (Eq, Ord, Show)

instance IsString Pat where
  fromString = PatAtom [] . fromString

mapNames :: ([Symbol] -> [Symbol]) -> Pat -> Pat
mapNames f pat = case pat of
  PatAtom      names l      -> PatAtom      (f names) l
  PatVar       names h      -> PatVar       (f names) h
  PatSeqVar    names ty     -> PatSeqVar    (f names) ty
  PatApp       names h cs   -> PatApp       (f names) h cs
  PatAlt       names p1 p2  -> PatAlt       (f names) p1 p2
  PatCondition names p expr -> PatCondition (f names) p expr

addNames :: [Symbol] -> Pat -> Pat
addNames xs = mapNames (xs++)

data SeqType = ZeroOrMore | OneOrMore
  deriving (Eq, Ord, Show)

pPrintNamed :: [Symbol] -> String -> String
pPrintNamed [] s = s
pPrintNamed (x:xs) s = concat
  [ pPrint x
  , ":("
  , pPrintNamed xs s
  , ")"
  ]

instance PPrint Pat where
  pPrint (PatAtom names l) = pPrintNamed names (pPrint l)
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
  pPrint (PatApp names f args) = pPrintNamed names $
    concat
    [ pPrint f
    , "["
    , intercalate ", " (map pPrint args)
    , "]"
    ]
  pPrint (PatAlt names p1 p2) = pPrintNamed names $
    pPrint p1 ++ "|" ++ pPrint p2
  pPrint (PatCondition names p t) = pPrintNamed names $
    pPrint p ++ "/;" ++ pPrint t

{-
exprToPat :: Expr -> Maybe Pat
exprToPat = undefined
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

symbolAppType :: Context -> Symbol -> AppType
symbolAppType ctx s =
  case (Set.member Flat attr, Set.member Orderless attr) of
    (False, False) -> AppFree
    (True,  False) -> AppA s Nothing
    (False, True ) -> AppC
    (True,  True ) -> AppAC s Nothing
  where
    attr = lookupAttributes s ctx

exprAppType :: Context -> Expr -> AppType
exprAppType ctx (ExprAtom (LitSymbol s)) = symbolAppType ctx s
exprAppType _   _                        = AppFree

data Binding
  = BindSingle Expr
  | BindSequence [Expr]
  deriving (Eq, Ord, Show)

data Substitution = MkSubstitution Symbol Binding
  deriving (Eq, Ord, Show)

newtype SubstitutionSet = MkSubstitutionSet (Map Symbol Binding)
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
lookupBinding :: Symbol -> SubstitutionSet -> Maybe Binding
lookupBinding s (MkSubstitutionSet m) = Map.lookup s m

-- | Replace the Symbols in the given Expr with their corresponding
-- Bindings in 'substSet'. If the binding is a BindSequence, we wrap
-- the given exprs in 'Sequence', which should be flattened by the
-- evaluator.
applySubstitutions :: SubstitutionSet -> Expr -> Expr
applySubstitutions substSet = mapSymbols $ \s ->
  case lookupBinding s substSet of
    Nothing -> ExprAtom (LitSymbol s)
    Just (BindSingle expr) -> expr
    Just (BindSequence exprs) -> ExprApp "Sequence" exprs

bindVars :: [Symbol] -> Expr -> [Substitution]
bindVars xs t = [MkSubstitution x (BindSingle t) | x <- xs]

bindSeqVars :: [Symbol] -> [Expr] -> [Substitution]
bindSeqVars xs ts = [MkSubstitution x (BindSequence ts) | x <- xs]

guardSeqTy :: Alternative f => SeqType -> [a] -> f ()
guardSeqTy OneOrMore [] = empty
guardSeqTy _         _  = pure ()

data MatchingEq
  = SingleEq Pat Expr
  | SeqEq AppType [Pat] [Expr]
  deriving (Eq, Ord, Show)

-- | All splits of xs into (t1,t2) where xs == t1++t2
-- 
-- Examples:
-- > split [2,3] = ([],[2,3]), ([2],[3]), ([2,3],[])
-- > split [1,2,3] = ([],[1,2,3]), ([1],[2,3]), ([1,2],[3]), ([1,2,3],[])
split :: [a] -> [([a],[a])]
split [] = [([], [])]
split (x:xs) = ([], x:xs) : do
  (xs', ys') <- split xs
  pure (x : xs', ys')

-- | All splits of xs into (t1, t, t2) where xs == t1++[t]++t2.
--
-- Examples:
-- > split1 [1,2] = ([],1,[2]), ([1],2,[])
split1 :: [a] -> [([a], a, [a])]
split1 xs = do
  (l, x:r) <- split xs
  pure (l,x,r)

-- | Split xs into all pairs (subSeq,rest) where subSeq is a
-- subsequence of xs and rest are the remaining elements not in
-- subSeq, with order preserved.
--
-- Examples:
-- > subSequences [1,2] = [([], [1,2]), ([1],[2]), ([2],[1]), ([1,2],[])]
--
-- Note that the number of return values is 2^(length xs). This could
-- introduce performance issues, so should be used with care.
subSequences :: [a] -> [([a], [a])]
subSequences [] = [([], [])]
subSequences (x:xs) = do
  (s,rest) <- subSequences xs
  [(x:s, rest), (s, x:rest)]

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
    matchesHead Nothing _ = True
    matchesHead (Just s) (ExprApp (ExprAtom (LitSymbol s')) _) = s == s'
    matchesHead (Just "Integer")  (ExprAtom (LitInteger _))    = True
    matchesHead (Just "Rational") (ExprAtom (LitRational _))   = True
    matchesHead (Just "String")   (ExprAtom (LitString _))     = True
    matchesHead (Just "Symbol")   (ExprAtom (LitSymbol _))     = True
    matchesHead _ _ = False

transformMatch :: Context -> MatchingEq -> [MatchStep]
transformMatch ctx eq = case eq of

  -- | T: Trivial
  SingleEq (PatAtom xs s) expr@(ExprAtom s')
    | s == s' -> [MatchBranch (bindVars xs expr) []]
    | otherwise -> []

  -- | IVE: Individual variable elimination
  SingleEq (PatVar x xHead) t -> checkHead xHead t (MatchBranch (bindVars x t) [])

  -- | A symbol cannot match with an ExprApp
  SingleEq (PatAtom _ _) (ExprApp _ _) -> []

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
  SeqEq _ [] [] -> [MatchBranch [] []]

  -- | SVE: Sequence variable elimination (applies under any head)
  SeqEq appTy (PatSeqVar x seqTy : ss) ts -> do
    (ts1, ts2) <- split ts
    guardSeqTy seqTy ts1
    pure $ MatchBranch (bindSeqVars x ts1) [SeqEq appTy ss ts2]

  -- | Dec-F: Decomposition under Free head
  SeqEq AppFree (s : ss) (t : ts) ->
    [MatchBranch [] [SingleEq s t, SeqEq AppFree ss ts]]

  -- | Commutative head
  --
  -- | SVE-C: Sequence variable elimination under commutative
  -- head. Dropped in Mathematica -- use SVE instead.
  --
  -- | Dec-C: Decomposition under commutative head
  SeqEq AppC (s : ss) ts -> do
    (ts1, t, ts2) <- split1 ts
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
  SeqEq (AppA f marking) (s : ss) ts ->
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
        t : ts'
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
          (ts1@(_:_), ts2) <- split ts
          guard $ not $ marking == Just Mark0 && length ts1 <= 1
          let
            xExpr = ExprApp (ExprAtom (LitSymbol f)) ts1
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
  -- only differences being uncons vs split1 for Dec-A vs Dec-AC, and
  -- split vs subSequences for IVE-A-strict vs IVE-AC-strict. We could
  -- try to deduplicate the code.
  --
  SeqEq (AppAC f marking) (s : ss) ts ->
    concat
    [
      -- | Dec-AC: Decomposition under AC head. Marking rules the same as AppA.
      -- TODO: Is this already strict? 
      case marking of
        Just Mark1 -> []
        _ -> do
          (ts1, t, ts2) <- split1 ts
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
          (subSeq@(_:_), rest) <- subSequences ts
          guard $ not $ marking == Just Mark0 && length subSeq <= 1
          let
            xExpr = ExprApp (ExprAtom (LitSymbol f)) subSeq
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
checkTrue ctx expr = eval ctx expr == "True"

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

tryApplyRule :: Context -> Rule -> Expr -> Maybe Expr
tryApplyRule ctx rule expr = case rule of
  PatRule pat rhs -> case solveMatch ctx (SingleEq pat expr) of
    []              -> Nothing
    (substSet : _) -> Just (applySubstitutions substSet rhs)
  BuiltinRule f -> f expr

flattenWithHead :: Expr -> [Expr] -> [Expr]
flattenWithHead h exprs = do
  expr <- exprs
  case expr of
    ExprApp h' args | h' == h -> flattenWithHead h args
    _                         -> pure expr

-- | Flatten any occurrences of Sequence[...] in the given list of
-- expressions. Note that this works with nested Sequence as well,
-- e.g.  flattenSequences [Sequence[Sequence[a]],b] -> [a,b]
flattenSequences :: [Expr] -> [Expr]
flattenSequences exprs = do
  expr <- exprs
  case expr of
    ExprApp "Sequence" seqExprs -> flattenSequences seqExprs
    _                           -> pure expr

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
      -- Evaluate the head and children, and flatten any sequences
      -- appearing in the children.
      --
      -- TODO: if h' has attributes SequenceHold or HoldAllComplete,
      -- then don't flatten Sequence's.
      h' = eval ctx h
      cs' = flattenSequences $ map (eval ctx) cs

      -- If h' is a symbol with attribute Flat, then flatten h' in
      -- children. If h' is a symbol with attribute Orderless, then
      -- subsequently sort the children.
      cs'' = case h' of
        ExprAtom (LitSymbol s) ->
          let
            attr = lookupAttributes s ctx
            maybeFlatten = if Set.member Flat attr      then flattenWithHead h' else id
            maybeSort    = if Set.member Orderless attr then sort               else id
          in
            maybeSort (maybeFlatten cs')
        _ -> cs'

      expr' = ExprApp h' cs''
      
      tryRules [] = Nothing
      tryRules (rule : rules) = case tryApplyRule ctx rule expr' of
        -- Check that the transformation doesn't result in an
        -- identical expression. If it does, we'd get an infinite loop
        -- if we evaluated again.
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

rational :: Rational -> Expr
rational = ExprAtom . LitRational

var :: Text -> Pat
var name = PatVar [MkSymbol name] Nothing

blankVar :: Pat
blankVar = PatVar [] Nothing

myExpr :: Expr
myExpr =
  ExprApp
  "a"
  [ 12
  , "c"
  , ExprApp
    "d"
    [ rational (17/2)
    ]
  ]

myPat :: Pat
myPat =
  PatApp ["y"]
  (var "x")
  [ blankVar
  , PatSeqVar ["A","A1"] OneOrMore
  , PatSeqVar ["B"] ZeroOrMore
  ]

(!) :: Expr -> [Expr] -> Expr
(!) = ExprApp

(!>) :: Pat -> [Pat] -> Pat
(!>) = PatApp []

myRHS :: Expr
myRHS = "y"!["A", "foo", "B"]

mySubstSet :: SubstitutionSet
mySubstSet = MkSubstitutionSet $
  Map.insert "c" (BindSequence ["e", "f"]) Map.empty

myRule :: Rule
myRule = PatRule lhs rhs
  where
    lhs = "f"!>[var "x"]
    rhs = "Plus"!["x", 12]

myRule2 :: Rule
myRule2 = BuiltinRule f
  where
    f expr = case expr of
      (ExprApp "Plus"
        [ExprAtom (LitInteger i), ExprAtom (LitInteger j)]) -> Just (ExprAtom (LitInteger (i+j)))
      _ -> Nothing

myContext :: Context
myContext = MkContext $ Map.fromList
  [ ( "f"
    ,  MkSymbolRecord
      { ownValue = Nothing
      , downValues = [myRule]
      , upValues = []
      , attributes = Set.empty
      }
    )
  , ( "Plus"
    ,  MkSymbolRecord
      { ownValue = Nothing
      , downValues = [myRule2]
      , upValues = []
      , attributes = Set.fromList [Flat, Orderless]
      }
    )
  , ( "Times"
    ,  MkSymbolRecord
      { ownValue = Nothing
      , downValues = []
      , upValues = []
      , attributes = Set.fromList [Flat, Orderless]
      }
    )
  ]
