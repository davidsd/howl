{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Howl.StdLib where

import Control.Monad          (guard, void)
import Control.Monad.IO.Class (liftIO)
import Data.FileEmbed         (embedFile, makeRelativeToProject)
import Data.Foldable          qualified as Foldable
import Data.Map.Strict        (Map)
import Data.Map.Strict        qualified as Map
import Data.Sequence          (Seq, pattern (:<|), pattern (:|>), pattern Empty)
import Data.Sequence          qualified as Seq
import Data.Set               qualified as Set
import Data.String            (fromString)
import Data.Text              (Text)
import Data.Text              qualified as Text
import Data.Text.Encoding     qualified as TE
import Math.Combinat          (binomial, multinomial)
import Howl.Eval         (MatchingEq (..), Substitution (..),
                               SubstitutionSet, emptySubstitutionSet, eval,
                               insertSubstitution, insertSubstitutions,
                               lookupBinding, removeBindings,
                               singletonSubstitutionSet, solveMatchMaybe,
                               tryApplyRule)
import Howl.Eval.Context (Attributes (..), Decl (..), Eval (..),
                               HoldType (..), Rule (..), addDecl, clear,
                               clearAll, compilePat, getDefinedSymbols,
                               lookupAttributes, lookupSymbolRecord,
                               modifyAttributes, newModuleSymbol, setFlat,
                               setHoldType, setNumericFunction, setOrderless)
import Howl.Expr         (Expr (..), FromExpr (..), Numeric (..),
                               ToExpr (..), bigFloatPrecision, pattern (:@),
                               pattern And, pattern ExprBigFloat,
                               pattern ExprDouble, pattern ExprInteger,
                               pattern ExprNumeric, pattern ExprRational,
                               pattern ExprString, pattern ExprView,
                               pattern List, pattern Null, pattern Or,
                               pattern Plus, pattern Power, pattern Set,
                               pattern Slot, pattern TagSetDelayed,
                               pattern Times, toBigFloat, toDouble)
import Howl.Expr         qualified as Expr
import Howl.Expr.TH      (declareBuiltins)
import Howl.Parser       (parseExprText, readExprFile)
import Howl.Pat          (patRootSymbol)
import Howl.Symbol       (Symbol)
import Howl.ToBuiltin    (ToBuiltin (..), Variadic (..), builtinDecl)
import Howl.Util         (pattern Pair, pattern Solo)
import Numeric.Rounded.Simple qualified as Rounded

---------- Plus ----------

data PlusArguments = MkPlusArguments
  { plusNum    :: !Numeric
  , plusOthers :: !(Map Expr Numeric)
  } deriving (Eq, Ord, Show)

addCoeffMap :: Ord a => a -> Numeric -> Map a Numeric -> Map a Numeric
addCoeffMap k n = Map.alter (Just . (+ n) . maybe 0 id) k

emptyPlusArguments :: PlusArguments
emptyPlusArguments = MkPlusArguments 0 Map.empty

addPlusArgument :: Expr -> PlusArguments -> PlusArguments
addPlusArgument arg plusArgs = case arg of
  ExprNumeric n -> plusArgs { plusNum = n + plusArgs.plusNum }
  _             -> plusArgs
    { plusOthers =
      let (term, coeff) = case arg of
            Times :@ Pair (ExprNumeric n) t   -> (t, n)
            Times :@ (ExprNumeric n :<| rest) -> (Times :@ rest, n)
            _                                 -> (arg, 1)
      in
        addCoeffMap term coeff plusArgs.plusOthers
    }

normalizePlus :: Seq Expr -> Expr
normalizePlus initialArgs =
  case allTerms of
    Empty  -> ExprInteger 0
    Solo t -> t
    _      -> Plus :@ (Seq.unstableSort $ Expr.flattenWithHead Plus allTerms)
  where
    plusArgs =
      foldr addPlusArgument emptyPlusArguments initialArgs
    numericTerm = toExpr plusArgs.plusNum
    otherTerms = Seq.fromList $ do
      (a, c) <- Map.toList plusArgs.plusOthers
      case (a, c) of
        (_, 0)              -> []
        (_, 1)              -> pure a
        (Times :@ terms, _) -> pure $ Times :@ (toExpr c :<| terms)
        _                   -> pure $ Times :@ Pair (toExpr c) a
    allTerms =
      (if numericTerm == ExprInteger 0 then id else (numericTerm :<|)) $
      otherTerms

---------- Times ----------

data TimesArguments = MkTimesArguments
  { timesNum  :: !Numeric
  , powerArgs :: !(Map Expr (Seq Expr))
  }

emptyTimesArguments :: TimesArguments
emptyTimesArguments = MkTimesArguments 1 Map.empty

addTimesArgument :: Expr -> TimesArguments -> TimesArguments
addTimesArgument arg timesArgs = case arg of
  ExprNumeric n -> timesArgs { timesNum = n * timesArgs.timesNum }
  _             -> timesArgs
    { powerArgs =
      let
        (base, ex) = case arg of
          Power :@ Pair a b -> (a, b)
          _                 -> (arg, ExprInteger 1)
      in
        Map.alter (Just . (ex :<|) . maybe Empty id) base timesArgs.powerArgs
    }

normalizeTimes :: Seq Expr -> Expr
normalizeTimes initialArgs
  | timesArgs.timesNum == 0 = ExprInteger 0
  | otherwise = case allTerms of
      Empty  -> ExprInteger 1
      Solo t -> t
      _      -> Times :@ (Seq.unstableSort $ Expr.flattenWithHead Times allTerms)
  where
    timesArgs = foldr addTimesArgument emptyTimesArguments initialArgs
    numericTerm = toExpr timesArgs.timesNum
    powerTerms = Seq.fromList $ do
      (a, bs) <- Map.toList timesArgs.powerArgs
      guard $ bs /= Seq.singleton (ExprInteger 0)
      pure $ normalizePower a $ normalizePlus bs
    allTerms =
      (if numericTerm == ExprInteger 1 then id else (numericTerm :<|)) $
      powerTerms

---------- Power ----------

extractFromExprs :: FromExpr a => Seq Expr -> (Seq a, Seq Expr)
extractFromExprs = go Empty Empty
  where
    go as bs Empty = (as, bs)
    go as bs (x :<| xs)
      | Just y <- fromExpr x = go (as :|> y) bs xs
      | otherwise            = go as (bs :|> x) xs

normalizePower :: Expr -> Expr -> Expr
normalizePower a b = case (a, b) of
  (ExprInteger 1, _) -> ExprInteger 1
  (_, ExprInteger 0) -> ExprInteger 1
  (_, ExprInteger 1) -> a
  (ExprNumeric na, ExprNumeric nb) -> numericPower na nb
  -- If the power is an integer, then expand by taking the power of each factor
  (Times :@ exprs, i@(ExprInteger _)) -> Times :@ (fmap (flip normalizePower i) exprs)
  -- Otherwise, we factor out the numeric part
  (Times :@ exprs, _) ->
    let
      (numericTerms, rest) = extractFromExprs exprs
      n = product numericTerms
      nAbs = abs n
    in
      case (n, signum n) of
        (1, _)  -> Expr.binary Power (Times :@ rest) b
        (_, 0)  -> Expr.binary Power (ExprInteger 0) b
        (-1,_)  -> Expr.binary Power (Times :@ (ExprInteger (-1) :<| rest)) b
        (_, -1) ->
          Times :@ Pair (normalizePower (ExprNumeric nAbs) b) (Expr.binary Power (Times :@ (ExprInteger (-1) :<| rest)) b)
        (_, _)  ->
          Times :@ Pair (normalizePower (ExprNumeric n) b) (Expr.binary Power (Times :@ rest) b)
  (Power :@ Pair expr y@(ExprNumeric _), _) ->
    Power :@ Pair expr (normalizeTimes (Pair y b))
  (_, _) -> Expr.binary Power a b

-- TODO: Detect exact rational powers, e.g. Sqrt[12] -> 2*Sqrt[3]
numericPower :: Numeric -> Numeric -> Expr
numericPower nx ny = case (nx, ny) of
  (NDouble   x, _) -> toExpr $ x ** toDouble ny
  (_, NDouble   y) -> toExpr $ toDouble nx ** y
  (NBigFloat x, NBigFloat y) -> toExpr $ bigFloatPow x y
  (NBigFloat x, _) -> toExpr $ bigFloatPow x (toBigFloat (bigFloatPrecision x) ny)
  (_, NBigFloat y) -> toExpr $ bigFloatPow (toBigFloat (bigFloatPrecision y) nx) y
  (NInteger  x, NInteger  y)
    | y >= 0    -> ExprInteger $ x^y
    | otherwise -> ExprRational $ toRational x^^y
  (NInteger  x, NRational y) -> Expr.binary Power (ExprInteger x) (ExprRational y)
  (NRational x, NInteger  y) -> ExprRational $ x^^y
  (NRational x, NRational y) -> Expr.binary Power (ExprRational x) (ExprRational y)
  where
    bigFloatPow x y =
      let p = min (bigFloatPrecision x) (bigFloatPrecision y)
      in Rounded.pow_ Rounded.TowardNearest p x y

multinomialPowerExpand :: Expr -> Integer -> Maybe Expr
multinomialPowerExpand (Plus :@ terms) b =
  if b < 0
  then Nothing
  else
    Just $ Plus :@ do
    powers <- tuplesThatSumTo (fromIntegral (length terms)) b
    let
      coeff = multinomialCoeff powers
      powerTerms = Seq.zipWith normalizePower terms (fmap ExprInteger powers)
    pure $ Times :@ (ExprInteger coeff :<| powerTerms)
multinomialPowerExpand _ _ = Nothing

tuplesThatSumTo :: Integer -> Integer -> Seq (Seq Integer)
tuplesThatSumTo = go
  where
    go 0 0 = Solo Empty
    go 0 _ = Empty
    go k m = do
      i <- Seq.fromList [0 .. m]
      fmap (i :<|) $ go (k-1) (m-i)

-- | TODO: a faster implementation with special cases for binomial
multinomialCoeff :: Seq Integer -> Integer
multinomialCoeff xs = case xs of
  Empty    -> 1
  Solo _   -> 1
  Pair x y -> binomial (x+y) y
  _        -> multinomial (Foldable.toList xs)

---------- OrderedQ ----------

-- | OrderedQ[h[x1,...,xn]] tests whether the xi's are in the
-- canonical order defined by the Haskell ordering on expressions
-- TODO: Implement a general predicate
orderedQ :: Expr -> Maybe Bool
orderedQ = \case
  _ :@ Empty      -> Just $ True
  _ :@ (x :<| xs) -> Just $ go x xs
  _               -> Nothing
  where
    go _ Empty = True
    go prev (y :<| ys)
      | prev <= y = go y ys
      | otherwise = False

---------- SameQ ----------

sameQ :: Seq Expr -> Bool
sameQ = \case
  Empty      -> True
  x :<| rest -> all (== x) rest

---------- And ----------

normalizeAnd :: Seq Expr -> Expr
normalizeAnd args = case filterBools args of
  Nothing       -> Expr.False
  Just Empty    -> Expr.True
  Just (Solo x) -> x
  Just xs       -> And :@ xs
  where
    filterBools Empty                = Just Empty
    filterBools (Expr.False :<| _)   = Nothing
    filterBools (Expr.True :<| rest) = filterBools rest
    filterBools (x :<| xs)           = fmap (x :<|) (filterBools xs)

---------- Or ----------

normalizeOr :: Seq Expr -> Expr
normalizeOr args = case filterBools args of
  Nothing       -> Expr.True
  Just Empty    -> Expr.False
  Just (Solo x) -> x
  Just xs       -> Or :@ xs
  where
    filterBools Empty                 = Just Empty
    filterBools (Expr.True :<| _)     = Nothing
    filterBools (Expr.False :<| rest) = filterBools rest
    filterBools (x :<| xs)            = fmap (x :<|) (filterBools xs)

---------- Comparisons ----------

lessDef :: Expr -> Expr -> Maybe Bool
lessDef (ExprNumeric a) (ExprNumeric b) = Just $ a < b
lessDef x y | x == y                    = Just False
lessDef _ _                             = Nothing

lessEqualDef :: Expr -> Expr -> Maybe Bool
lessEqualDef (ExprNumeric a) (ExprNumeric b) = Just $ a <= b
lessEqualDef x y | x == y                    = Just True
lessEqualDef _ _                             = Nothing

greaterDef :: Expr -> Expr -> Maybe Bool
greaterDef (ExprNumeric a) (ExprNumeric b) = Just $ a > b
greaterDef x y | x == y                    = Just False
greaterDef _ _                             = Nothing

greaterEqualDef :: Expr -> Expr -> Maybe Bool
greaterEqualDef (ExprNumeric a) (ExprNumeric b) = Just $ a >= b
greaterEqualDef x y | x == y                    = Just True
greaterEqualDef _ _                             = Nothing

equalDef :: Expr -> Expr -> Maybe Bool
equalDef (ExprNumeric a) (ExprNumeric b) = Just $ a == b
equalDef x y | x == y                    = Just True
equalDef _ _                             = Nothing

-- ========== Scoped constructs ========== --

$(declareBuiltins ''Expr 'fromString
  [ "Function"
  , "Let"
  , "Module"
  , "Table"
  ])

-- | A datatype that matches a single expression e or a list of
-- expressions {e1,...,en}, such that the expressions can all be
-- mapped to the type 'a'.
newtype ListOrSolo a = MkListOrSolo (Seq a)
instance FromExpr a => FromExpr (ListOrSolo a) where
  fromExpr = fmap MkListOrSolo . \case
    List :@ es -> mapM fromExpr es
    e          -> fmap pure $ fromExpr e

data TableRange
  = RangeLength Integer
  | RangeIndices Symbol Numeric Numeric Numeric
  | RangeList Symbol (Seq Expr)

instance FromExpr TableRange where
  fromExpr = \case
    List :@ Solo (ExprInteger i)
      -> Just $ RangeLength i
    List :@ (ExprSymbol x :<| ExprNumeric i :<| Empty)
      -> Just $ RangeIndices x 1 i 1
    List :@ (ExprSymbol x :<| ExprNumeric i :<| ExprNumeric j :<| Empty)
      -> Just $ RangeIndices x i j 1
    List :@ (ExprSymbol x :<| ExprNumeric i :<| ExprNumeric j :<| ExprNumeric k :<| Empty)
      -> Just $ RangeIndices x i j k
    List :@ (ExprSymbol x :<| (List :@ xs) :<| Empty)
      -> Just $ RangeList x xs
    _ -> Nothing

-- | Detects any new variables introduced by the given
-- expression. This is used to avoid overwriting shadowing variables
-- in constructs like Function, Let, Module, and Table.
--
-- For example, we should have:
--
-- --> Let[x=2, Function[x, Function[x,x+9]][12][x]]
-- --> Function[x, Function[x,x+9]][12][2]
-- --> Function[x,x+9][2]
-- --> 2 + 9
-- --> 11
--
introducedVariables :: Expr -> Seq Symbol
introducedVariables = \case
  Function :@ Pair (ExprView (MkListOrSolo vars))     _ -> vars
  Let      :@ Pair (ExprView (MkListOrSolo bindings)) _ -> fmap bindVar bindings
  Module   :@ Pair (ExprView (MkListOrSolo vars))     _ -> vars
  Table    :@ Pair _ (ExprView range) -> case range of
    RangeIndices var _ _ _ -> Solo var
    RangeList var _        -> Solo var
    _                      -> Empty
  _ -> Empty

-- | Replace the Symbols in the given Expr with their corresponding
-- Bindings in 'substSet', allowing new local variables introduced in
-- sub-expressions to shadow the given substitutions.
--
-- [NB Shadowing in patterns]: It would be nice if we could implement
-- shadowing for variables in patterns and rules. For example, morally
-- speaking we should not be able to modify the variable x inside the
-- rule:
--
-- x_ :> x+1
--
-- However, both Mathematica and Howl currently have:
--
-- (x_:>x+2)/.x->12 ---> Pattern[12, Blank[]] :> 12 + 2
--
-- It is difficult to get around this because x is introduced in a
-- pattern and then used on the RHS of a rule, and we would have to
-- enumerate all the places that a variable bound in a pattern might
-- show up. Maybe it's only in Rule, RuleDelayed, Set, SetDelayed,
-- TagSetDelayed, UpSet, UpSetDelayed?
applySubstitutionsWithShadowing :: SubstitutionSet -> Expr -> Expr
applySubstitutionsWithShadowing = go
  where
    go substSet expr = case expr of
      ExprApp h cs ->
        let
          newSubstSet = removeBindings (introducedVariables expr) substSet
          h' = go newSubstSet h
          cs' = fmap (go newSubstSet) cs
        in
          ExprApp h' cs'
      ExprSymbol sym
        | Just rhs <- lookupBinding sym substSet -> rhs
      _ -> expr

---------- Function ----------

-- | Function[x,body][y] is implemented by performing the symbolic
-- substitution body /. x->y *before evaluation*. As a consequence,
-- any occurences of x inside body will be substituted with y,
-- regardless of whether the symbol x is bound to anything outside the
-- scope of the Function. In other words, function variables exhibit
-- shadowing.
--
-- Meanwhile Function[body] is an anonymous function, where body
-- contains some number of Slot[_]'s. We implement it by replacing
-- Slot[i] inside body with the appropriate argument, but not inside
-- other anonymous functions. This avoids a situation like this:
--
-- Function[Slot[1]*Function[1+Slot[1]]][y] --> y*Function[1+y] (BAD)
--
-- Instead, we should have:
--
-- Function[Slot[1]*Function[1+Slot[1]]][y] -> y*Function[1+Slot[1]]
--
-- NB: When we apply substitutions, we need to be careful not to do it
-- inside a construct that introduces variables.
--
functionDef :: Expr -> Maybe Expr
functionDef = \case
  (Function :@ Solo body) :@ args -> Just $ replaceSlots args body
  (Function :@ Pair (ExprView (MkListOrSolo vars)) body) :@ args
    | Just bindings <- bindVars_maybe vars args
    -> Just $ applySubstitutionsWithShadowing bindings body
  _ -> Nothing
  where
    bindVars_maybe :: Seq Symbol -> Seq Expr -> Maybe SubstitutionSet
    bindVars_maybe Empty      Empty      = Just emptySubstitutionSet
    bindVars_maybe (a :<| as) (b :<| bs) =
      insertSubstitution (MkSubstitution a b) =<< bindVars_maybe as bs
    bindVars_maybe _          _          = Nothing

    replaceSlots :: Seq Expr -> Expr -> Expr
    replaceSlots vals = go
      where
        go (Slot :@ Solo (ExprInteger i))
          | Just val <- Seq.lookup (fromInteger i - 1) vals = val
          | otherwise = error $ "Slot[" <> show i <> "] not bound to anything"
        -- Important: do not replace inside an anonymous function!
        go expr@(Function :@ Solo _)      = expr
        go (h :@ args)                    = go h :@ fmap go args
        go expr                           = expr

-- | Note: functionDef is a curried definition: it does NOT match
-- something of the form Function[...]. Thus, we cannot use
-- 'builtinFunctionMaybeM' or 'function'. We need to construct the
-- Decl by hand.
functionDecl :: Decl
functionDecl = DownValue "Function" $ BuiltinRule (pure . functionDef)

---------- Let ----------

-- | A datatype for parsing a binding x=y, where x is a 'Symbol'
data SetBind = MkSetBind Symbol Expr
instance FromExpr SetBind where
  fromExpr = \case
    Set :@ Pair (ExprSymbol x) y -> Just $ MkSetBind x y
    _                            -> Nothing

bindVar :: SetBind -> Symbol
bindVar (MkSetBind x _) = x

-- | Let is a construct for creating local variables. It is different
-- from the scoping constructs in Mathematica. The variables in a Let
-- are locally defined, and they shadow global definitions. Subsequent
-- bindings can refer to previous bindings.
--
-- Let is defined by the following transformation:
--
-- Let[{}, expr]          ---> expr
-- Let[{x=x0,...}, expr]  ---> Function[x, Let[{...}, expr]][x0]
--
-- This creates a version of Let that shadows variables outside the
-- scope of the Let. For example:
--
-- x = 10;
-- Let[x=9, x] --> 9
--
-- Notice that subsequent Let bindings can refer to previously bound
-- variables:
--
-- Let[{x=9, y=x+1}, y] --> 10
--
letDef :: ListOrSolo SetBind -> Expr -> Expr
letDef (MkListOrSolo bindings) body = Foldable.foldr funApp body bindings
  where
    funApp (MkSetBind x x0) expr = Function :@ Pair (ExprSymbol x) expr :@ Solo x0

---------- Module ----------

moduleDef :: ListOrSolo Symbol -> Expr -> Eval (Maybe Expr)
moduleDef (MkListOrSolo vars) body = go vars
  where
    go varSymbols = do
      modVars <- mapM newModuleSymbol varSymbols
      pure $ do
        let substs = Seq.zipWith MkSubstitution varSymbols (fmap ExprSymbol modVars)
        substSet <- insertSubstitutions substs emptySubstitutionSet
        pure $ applySubstitutionsWithShadowing substSet body

---------- Map ----------

mapDef :: Expr -> Expr -> Maybe Expr
mapDef = \cases
  f (h :@ xs) -> Just $ h :@ (fmap (Expr.unary f) xs)
  _ _ -> Nothing

-- TODO: multiple indices
mapAt :: Expr -> Expr -> Int -> Maybe Expr
mapAt fExpr (ExprApp h cs) n
  | n == 0
  = Just $ ExprApp (Expr.unary fExpr h) cs
  | n > 0 && n <= Seq.length cs
  = Just $ ExprApp h $ Seq.adjust' (Expr.unary fExpr) (n-1) cs
  | n < 0 && -n <= Seq.length cs
  = Just $ ExprApp h $ Seq.adjust' (Expr.unary fExpr) (Seq.length cs + n) cs
  | otherwise = Nothing
mapAt _ _ _ = Nothing

---------- ReplaceAll ----------

$(declareBuiltins ''Expr 'fromString
  [ "RuleDelayed"
  , "Rule"
  ])

-- | Either a Rule or RuleDelayed. These are treated in exactly the
-- same way -- the only difference being that RuleDelayed has
-- attribute HoldAll, while Rule has attribute HoldFirst. (In
-- Mathematica, Rule and RuleDelayed do not hold their first
-- arguments, but we differ here to avoid evaluating patterns.)
data ARule = MkRule Expr Expr
instance FromExpr ARule where
  fromExpr = \case
    Rule        :@ Pair lhs rhs -> Just $ MkRule lhs rhs
    RuleDelayed :@ Pair lhs rhs -> Just $ MkRule lhs rhs
    _ -> Nothing

-- | NB: replaceAll can currently be used to subvert shadowing
-- variables by replacing them with arbitrary expressions. For
-- example:
--
-- > bar = Function[x,x+2]
-- > bar /. x:>10
-- >>> Function[10,10+2]
--
-- Mathematica also has this problem. TODO: Should we fix it? The fix
-- probably involves using applySubstitutionsWithShadowing inside Eval
-- instead of applySubstitutions. Note that a workaround is to use
-- anonymous functions:
--
-- > bar = (#+2 &)
-- > bar /. x:>10
-- >>> Function[Slot[1]+2]
--
replaceAll :: Expr -> ListOrSolo ARule -> Eval Expr
replaceAll e (MkListOrSolo rules) = do
  rules' <- traverse aruleToRule rules
  go rules' e
  where
    aruleToRule :: ARule -> Eval Rule
    aruleToRule (MkRule lhs rhs) = do
      pat <- compilePat lhs
      pure $ PatRule pat rhs

    -- Repeatedly try rules in the given Sequence until one of them
    -- works
    tryRules _ Empty = pure Nothing
    tryRules expr (r :<| rs) = tryApplyRule r expr >>= \case
      result@(Just _) -> pure result
      Nothing         -> tryRules expr rs

    go :: Seq Rule -> Expr -> Eval Expr
    go rules' expr = tryRules expr rules' >>= \case
      Just result -> pure result
      -- If none of the rules work, go for the head and children
      Nothing -> case expr of
        h :@ cs -> do
          h' <- go rules' h
          cs' <- traverse (go rules') cs
          pure $ h' :@ cs'
        _       -> pure expr

---------- ReplaceRepeated ----------

replaceRepeated :: Expr -> ListOrSolo ARule -> Eval Expr
replaceRepeated expr rules = go expr
  where
    go currentExpr = do
      newExpr <- replaceAll currentExpr rules >>= eval
      if newExpr /= currentExpr
        then go newExpr
        else pure newExpr

---------- Length ----------

lengthDef :: Expr -> Int
lengthDef = \case
  ExprApp _ xs -> Seq.length xs
  _            -> 0

---------- Part ----------

-- | This matches Mathematica's behavior where it only returns a Part
-- if all the arguments are integers in the correct range. For example
-- f[g[h]][[1,foo]] does not simplify to g[h][[foo]].
part :: Seq Expr -> Maybe Expr
part Empty = Nothing
part (expr :<| indices) = go expr indices
  where
    go e Empty = Just e
    -- This rule leads to some funny behavior that nonetheless matches
    -- Mathematica's behavior::
    --
    -- f[g][[{0, 0, 0}]] ---> f[f,f,f]
    -- 1[[{0, 0, 0}]]    ---> Integer[Integer, Integer, Integer]
    --
    go e (List :@ inds :<| inds') = do
      eHead <- exprHead e
      ExprApp eHead
        <$> mapM (\i -> go e (i :<| inds')) inds
    go (ExprApp h' cs) (ExprInteger i :<| inds')
      | i == 0
      = go h' inds'
      | Just c <- Seq.lookup (fromInteger i-1) cs
      = go c inds'
      | i < 0
      , let idx = Seq.length cs + fromInteger i
      , Just c <- Seq.lookup idx cs
      = go c inds'
      | otherwise = Nothing
    go _ (Solo (ExprInteger 0)) = exprHead expr
    go _ _ = Nothing

exprHead :: Expr -> Maybe Expr
exprHead (ExprApp h _)    = Just h
exprHead (ExprSymbol _)   = Just "Symbol"
exprHead (ExprInteger _)  = Just "Integer"
exprHead (ExprRational _) = Just "Rational"
exprHead (ExprDouble _)   = Just "Double"
exprHead (ExprBigFloat _) = Just "BigFloat"
exprHead (ExprString _)   = Just "String"
exprHead _                = Nothing

---------- Table ----------

table :: Expr -> TableRange -> Expr
table body range = case range of
  RangeLength n -> List :@ Seq.replicate (fromIntegral n) body
  RangeIndices xVar i j k -> List :@ Seq.unfoldr go i
    where
      go x
        | x > j = Nothing
        | otherwise =
          let
            subst = singletonSubstitutionSet xVar (ExprNumeric x)
            body' = applySubstitutionsWithShadowing subst body
          in
            Just (body', x + k)
  RangeList xVar xs -> List :@ fmap substX xs
    where
      substX x = applySubstitutionsWithShadowing (singletonSubstitutionSet xVar x) body


---------- Take/Drop ----------

newtype AList a = MkList { unList :: Seq a }

instance FromExpr a => FromExpr (AList a) where
  fromExpr = \case
    List :@ xs -> fmap MkList (mapM fromExpr xs)
    _          -> Nothing

instance ToExpr a => ToExpr (AList a) where
  toExpr (MkList xs) = List :@ fmap toExpr xs

takeDef :: AList Expr -> Int -> Maybe (AList Expr)
takeDef (MkList xs) n
  | n <= Seq.length xs = Just $ MkList (Seq.take n xs)
  | otherwise          = Nothing

dropDef :: AList Expr -> Int -> Maybe (AList Expr)
dropDef (MkList xs) n
  | n <= Seq.length xs = Just $ MkList (Seq.drop n xs)
  | otherwise          = Nothing

---------- Accumulate ----------

accumulateDef :: AList Expr -> AList Expr
accumulateDef (MkList xs) = MkList $ case xs of
  Empty      -> Empty
  x :<| rest -> snd $ Foldable.foldl' step (x, Solo x) rest
  where
    step (acc, out) y =
      let acc' = normalizePlus (Pair acc y)
      in (acc', out :|> acc')

firstDef :: Expr -> Maybe Expr
firstDef = \case
  ExprApp _ (x :<| _) -> Just x
  _                   -> Nothing

restDef :: Expr -> Maybe Expr
restDef = \case
  ExprApp h (_ :<| xs) -> Just (ExprApp h xs)
  _                    -> Nothing

---------- Reverse ----------

reverseDef :: Expr -> Maybe Expr
reverseDef = \case
  ExprApp h xs -> Just $ ExprApp h (Seq.reverse xs)
  _            -> Nothing

---------- Union ----------

union :: Seq (AList Expr) -> AList Expr
union = MkList . Seq.fromList . Set.toList . Set.unions . fmap toSet
  where
    toSet (MkList xs) = Set.fromList (Foldable.toList xs)

---------- Intersection ----------

intersection :: Seq (AList Expr) -> AList Expr
intersection = MkList . Seq.fromList . Set.toList . intersectAll . fmap toSet
  where
    toSet (MkList xs) = Set.fromList (Foldable.toList xs)
    intersectAll = \case
      Empty    -> Set.empty
      s :<| ss -> Foldable.foldl' Set.intersection s ss

---------- Ordering ----------

ordering :: AList Expr -> AList Int
ordering (MkList xs) = MkList $
  fmap (succ . snd) $
  Seq.unstableSortOn fst $
  Seq.mapWithIndex (\i x -> (x,i)) xs

orderingN :: AList Expr -> Int -> AList Int
orderingN xs n = MkList . Seq.take n . (.unList) . ordering $ xs

---------- Sort ----------

sortDef :: AList Expr -> AList Expr
sortDef (MkList xs) = MkList $ Seq.unstableSort xs

---------- Count and Position ----------

-- TODO: Level specification. To do this, we need a way of declaring
-- only certain arguments to be Held. Basically we need a HoldArgs
-- (Set Int) constructor for HoldType. Then count and position would
-- have hold type HoldArgs (Set.singleton 2).
count :: Expr -> Expr -> Eval Int
count expr patExpr = case expr of
  ExprApp _ xs -> do
    pat <- compilePat patExpr
    let
      go e i = do
        maybeMatch <- solveMatchMaybe (SingleEq pat e)
        pure $ case maybeMatch of
          Just _  -> i+1
          Nothing -> i
    Foldable.foldrM go 0 xs
  _ -> pure 0

position :: Expr -> Expr -> Eval (AList (AList Int))
position expr patExpr = do
  pat <- compilePat patExpr
  case expr of
    ExprApp h xs -> do
      let
        go :: Seq Int -> Int -> Seq Expr -> Eval (Seq Int)
        go acc _ Empty = pure acc
        go acc i (y :<| ys) = do
          maybeMatch <- solveMatchMaybe (SingleEq pat y)
          case maybeMatch of
            Just _  -> go (acc :|> i) (i+1) ys
            Nothing -> go acc (i+1) ys
      positions <- go Empty 0 (h :<| xs)
      pure $ MkList $ fmap (MkList . Solo) positions
    _ -> do
      maybeMatch <- solveMatchMaybe (SingleEq pat expr)
      -- No idea why, but this matches Mathematica's behavior
      pure $ case maybeMatch of
        Just _  -> MkList (Solo (MkList Empty))
        Nothing -> MkList Empty

---------- Flatten ----------

-- TODO: Level specification
flatten :: Expr -> Maybe Expr
flatten = \case
  ExprApp h cs -> Just $ ExprApp h (Expr.flattenWithHead h cs)
  _            -> Nothing

---------- ConfirmPatternTest ----------

confirmPatternTest :: Seq Expr -> Maybe Expr
confirmPatternTest = \case
  Empty         -> Nothing
  test :<| rest -> Just $ And :@ (fmap (Expr.unary test) rest)

---------- NumericFunctionQ ----------

numericFunctionQ :: Symbol -> Eval Bool
numericFunctionQ = fmap (.numericFunction) . lookupAttributes

-- | TODO: It is probably unnecessary to declare these all in this big
-- block. Remove them from the block as we define them one-by-one.
$(declareBuiltins ''Expr 'fromString
   [ "Abs"
   , "Sign"
   , "Pi"
   , "E"
   , "Exp"
   , "Log"
   , "Sin"
   , "Cos"
   , "Tan"
   , "ArcSin"
   , "ArcCos"
   , "ArcTan"
   , "Sinh"
   , "Cosh"
   , "Tanh"
   , "ArcSinh"
   , "ArcCosh"
   , "ArcTanh"
   ])

builtinNumericFunctions :: [Symbol]
builtinNumericFunctions =
 [ "Power", "Plus", "Times", "Exp", "Log", "Sin", "Cos"
 , "Tan", "ArcSin", "ArcCos", "ArcTan", "Sinh", "Cosh"
 , "Tanh", "ArcSinh", "ArcCosh", "ArcTanh"
 ]

---------- SetDelayed and Set ----------

data LHS
  = LHSSymbol Symbol
  | LHSPat Expr
  | LHSTaggedPat Symbol Expr
  deriving (Show)

-- | TODO: Add UpSet
instance FromExpr LHS where
  fromExpr = \case
    ExprSymbol sym                                   -> Just $ LHSSymbol sym
    TagSetDelayed :@ (Pair (ExprSymbol sym) patExpr) -> Just $ LHSTaggedPat sym patExpr
    patExpr                                          -> Just $ LHSPat patExpr

setPairToDecl :: LHS -> Expr -> Eval Decl
setPairToDecl lhs rhs = case lhs of
  LHSSymbol sym -> pure $ OwnValue sym rhs
  LHSPat patExpr -> do
    pat <- compilePat patExpr
    case patRootSymbol pat of
      Just sym -> pure $ DownValue sym (PatRule pat rhs)
      Nothing  -> error "Pattern on the left-hand side has no root symbol"
  LHSTaggedPat sym patExpr -> do
    pat <- compilePat patExpr
    pure $ UpValue sym (PatRule pat rhs)

-- | There are two differences between SetDelayed and Set. Firstly,
-- SetDelayed has attribute HoldAll, so that the rhs is unevaluated
-- when the rule is added to the Context. By contrast, Set has
-- attribute HoldFirst, so that the rhs (its second argument) is
-- evaluated before the rule is added to the Context. Secondly, 'Set'
-- returns the rhs, which has already been evaluated.
--
setDef :: LHS -> Expr -> Eval Expr
setDef lhs rhs = do
  setPairToDecl lhs rhs >>= addDecl
  pure rhs

setDelayedDef :: LHS -> Expr -> Eval ()
setDelayedDef lhs rhs = setPairToDecl lhs rhs >>= addDecl

---------- CompoundExpression ----------

-- | Since we don't give CompoundExpression any attributes, all of its
-- arguments will be pre-evaluated by the evaluator. All we have to do
-- here is return the final one.
compoundExpression :: Seq Expr -> Expr
compoundExpression = \case
  Empty       -> Null
  _ :|> final -> final

---------- Attributes ----------

newtype AttrModifier = MkAttrModifier { getModifier :: Attributes -> Attributes }

instance FromExpr AttrModifier where
  fromExpr = fmap MkAttrModifier . \case
    "Flat"      -> Just setFlat
    "Orderless" -> Just setOrderless
    "HoldAll"   -> Just $ setHoldType HoldAll
    "HoldFirst" -> Just $ setHoldType HoldFirst
    "HoldRest"  -> Just $ setHoldType HoldRest
    _           -> Nothing

setAttributes :: Symbol -> ListOrSolo AttrModifier -> Eval ()
setAttributes sym (MkListOrSolo attrs) =
  mapM_ (modifyAttributes sym . (.getModifier)) attrs

---------- Help ----------

help :: Symbol -> Eval ()
help sym = do
  maybeRecord <- lookupSymbolRecord sym
  liftIO $ putStrLn $ show maybeRecord

-- ========== Building Contexts ========== --

run :: Text -> Eval Expr
run input = case parseExprText input of
  Left err   -> liftIO (putStrLn err) >> pure Expr.Null
  Right expr -> eval expr

run_ :: Text -> Eval ()
run_ = void . run

get :: FilePath -> Eval Expr
get path = readExprFile path >>= eval

get_ :: FilePath -> Eval ()
get_ = void . get

def :: ToBuiltin f => Symbol -> f -> Eval ()
def sym f = addDecl $ builtinDecl sym f

stdLibWL :: Text
stdLibWL =
  TE.decodeUtf8
  $(makeRelativeToProject "wl/StdLib.wl" >>= embedFile)

defStdLib :: Eval ()
defStdLib = do
  modifyAttributes "Set" (setHoldType HoldFirst)
  def "Set" setDef

  modifyAttributes "SetDelayed" (setHoldType HoldAll)
  def "SetDelayed" setDelayedDef

  modifyAttributes "Hold" (setHoldType HoldAll)
  def "CompoundExpression" (MkVariadic compoundExpression)
  def "Get" (readExprFile @Eval . Text.unpack)
  def "SetAttributes" setAttributes
  def "Clear" clear
  def "ClearAll" clearAll
  def "Help" help
  def "DefinedSymbols" getDefinedSymbols

  modifyAttributes "Function" (setHoldType HoldAll)
  addDecl functionDecl

  modifyAttributes "Let" (setHoldType HoldAll)
  def "Let" letDef

  modifyAttributes "Module" (setHoldType HoldAll)
  def "Module" moduleDef

  modifyAttributes "And" setFlat
  def "And" (MkVariadic normalizeAnd)

  modifyAttributes "Or" setFlat
  def "Or" (MkVariadic normalizeOr)

  def "Identity" (id @Expr)
  def "Equal" equalDef
  def "Greater" greaterDef
  def "Less" lessDef
  def "GreaterEqual" greaterEqualDef
  def "LessEqual" lessEqualDef

  modifyAttributes "Rule"        (setHoldType HoldFirst)
  modifyAttributes "RuleDelayed" (setHoldType HoldAll)
  def "ReplaceAll" replaceAll
  def "ReplaceRepeated" replaceRepeated
  def "ConfirmPatternTest" (MkVariadic confirmPatternTest)

  def "Map" mapDef
  def "MapAt" mapAt
  def "Head" exprHead
  def "Part" (MkVariadic part)
  def "Length" lengthDef
  def "Table" table
  def "Take" takeDef
  def "Drop" dropDef
  def "Accumulate" accumulateDef
  def "First" firstDef
  def "Rest" restDef
  def "Reverse" reverseDef
  def "Union" (MkVariadic union)
  def "Intersection" (MkVariadic intersection)
  def "Ordering" ordering
  def "Ordering" orderingN
  def "Sort" sortDef
  def "Flatten" flatten
  modifyAttributes "Count" (setHoldType HoldRest)
  def "Count" count
  modifyAttributes "Position" (setHoldType HoldRest)
  def "Position" position

  modifyAttributes "Plus" (setFlat . setOrderless)
  def "Plus" (MkVariadic normalizePlus)

  modifyAttributes "Times" (setFlat . setOrderless)
  def "Times" (MkVariadic normalizeTimes)

  def "Power" normalizePower
  def "Sqrt" $ \e -> normalizePower e (ExprRational (1/2))

  def "SameQ" (MkVariadic sameQ)
  def "OrderedQ" orderedQ
  def "EvenQ" (even @Integer)
  def "OddQ" (odd @Integer)

  sequence_ $ do
    numFn <- builtinNumericFunctions
    pure $ modifyAttributes numFn setNumericFunction
  def "NumericFunctionQ" numericFunctionQ

  def "NumericQ" $ \(_ :: Numeric) -> True
  def "MultinomialPowerExpand" multinomialPowerExpand

  run_ stdLibWL
