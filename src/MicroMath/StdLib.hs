{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module MicroMath.StdLib where

import Control.Monad       (guard)
import Control.Monad.State (MonadState)
import Data.Foldable       qualified as Foldable
import Data.Map.Strict     (Map)
import Data.Map.Strict     qualified as Map
import Data.Sequence       (Seq, pattern (:<|), pattern Empty)
import Data.Sequence       qualified as Seq
import Data.String         (fromString)
import Data.Text           (Text)
import Math.Combinat       (binomial, multinomial)
import MicroMath.Context   (Attributes (..), Context (..), Decl (..),
                            EvalM (..), HoldType (..), Rule (..), addDecl,
                            createContext, getContext, lookupAttributes,
                            modifyAttributes, newModuleSymbol, setFlat,
                            setHoldType, setNumericFunction, setOrderless)
import MicroMath.Eval      (Substitution (..), SubstitutionSet,
                            emptySubstitutionSet, eval, insertSubstitution,
                            insertSubstitutions, lookupBinding, removeBindings,
                            tryApplyRule)
import MicroMath.Expr      (Expr (..), FromExpr (..), Numeric (..), ToExpr (..),
                            builtinNumericFunctions, pattern (:@), pattern ExprView,
                            pattern And, pattern ExprInteger,
                            pattern ExprNumeric, pattern ExprRational,
                            pattern List, pattern Or, pattern Plus,
                            pattern Power, pattern Set, pattern SetDelayed,
                            pattern Slot, pattern TagSetDelayed, pattern Times)
import MicroMath.Expr      qualified as Expr
import MicroMath.Expr.TH   (declareBuiltin)
import MicroMath.Parser    (parseExprText)
import MicroMath.Pat       (patFromExpr, patRootSymbol)
import MicroMath.Symbol    (Symbol)
import MicroMath.Util      (pattern Pair, pattern Solo)
import MicroMath.ToBuiltin (ToBuiltin(..), builtinDecl)

---------- Numerics utilities ----------
-- TODO: Put these in a separate module? Maybe we need Num,
-- Fractional, etc. instances for Numeric types.

data Numerics = MkNumerics
  { integers  :: [Integer]
  , rationals :: [Rational]
  , reals     :: [Double]
  } deriving (Eq, Ord, Show)

emptyNumerics :: Numerics
emptyNumerics = MkNumerics [] [] []

addNumeric :: Numeric -> Numerics -> Numerics
addNumeric n nums = case n of
  NInteger i  -> nums { integers  = i:nums.integers }
  NRational r -> nums { rationals = r:nums.rationals }
  NReal r     -> nums { reals     = r:nums.reals }

collapseNumerics :: (forall a . Num a => [a] -> a) -> Numerics -> Expr
collapseNumerics f nums
  | _:_ <- nums.reals = toExpr $ f $
    nums.reals <>
    map realToFrac nums.rationals <>
    map realToFrac nums.integers
  | _:_ <- nums.rationals =
      toExpr $ f $ nums.rationals <> map toRational nums.integers
  | otherwise = toExpr $ f nums.integers

---------- Plus ----------

data PlusArguments = MkPlusArguments
  { plusNums   :: Numerics
  , plusOthers :: Map Expr Numerics
  } deriving (Eq, Ord, Show)

addNumericsMap :: Ord a => a -> Numeric -> Map a Numerics -> Map a Numerics
addNumericsMap k n = Map.alter (Just . addNumeric n . maybe emptyNumerics id) k

emptyPlusArguments :: PlusArguments
emptyPlusArguments = MkPlusArguments emptyNumerics Map.empty

addPlusArgument :: Expr -> PlusArguments -> PlusArguments
addPlusArgument arg plusArgs = case arg of
  ExprNumeric n -> plusArgs { plusNums   = addNumeric n plusArgs.plusNums }
  _             -> plusArgs
    { plusOthers =
      let (term, coeff) = case arg of
            Times :@ Pair (ExprNumeric n) t   -> (t, n)
            Times :@ (ExprNumeric n :<| rest) -> (Times :@ rest, n)
            _                                 -> (arg, NInteger 1)
      in
        addNumericsMap term coeff plusArgs.plusOthers
    }

normalizePlus :: Seq Expr -> Expr
normalizePlus initialArgs =
  case allTerms of
    Empty  -> 0
    Solo t -> t
    _      -> Plus :@ (Seq.unstableSort $ Expr.flattenWithHead Plus allTerms)
  where
    plusArgs =
      foldr addPlusArgument emptyPlusArguments initialArgs
    numericTerm = collapseNumerics sum plusArgs.plusNums
    otherTerms = Seq.fromList $ do
      (a, cs) <- Map.toList plusArgs.plusOthers
      let coeff = collapseNumerics sum cs
      case (a, coeff) of
        (_, 0)              -> []
        (_, 1)              -> pure a
        (Times :@ terms, _) -> pure $ Times :@ (coeff :<| terms)
        _                   -> pure $ Times :@ Pair coeff a
    allTerms =
      (if numericTerm == 0 then id else (numericTerm :<|)) $
      otherTerms

---------- Times ----------

data TimesArguments = MkTimesArguments
  { timesNums :: Numerics
  , powerArgs :: Map Expr (Seq Expr)
  }

emptyTimesArguments :: TimesArguments
emptyTimesArguments = MkTimesArguments emptyNumerics Map.empty

addTimesArgument :: Expr -> TimesArguments -> TimesArguments
addTimesArgument arg timesArgs = case arg of
  ExprNumeric n -> timesArgs { timesNums = addNumeric n timesArgs.timesNums }
  _             -> timesArgs
    { powerArgs =
      let
        (base, ex) = case arg of
          Power :@ Pair a b -> (a, b)
          _                 -> (arg, 1)
      in
        Map.alter (Just . (ex :<|) . maybe Empty id) base timesArgs.powerArgs
    }

normalizeTimes :: Seq Expr -> Expr
normalizeTimes initialArgs =
  case allTerms of
    Empty  -> 1
    Solo t -> t
    _      -> Times :@ (Seq.unstableSort $ Expr.flattenWithHead Times allTerms)
  where
    timesArgs = foldr addTimesArgument emptyTimesArguments initialArgs
    numericTerm = collapseNumerics product timesArgs.timesNums
    powerTerms = Seq.fromList $ do
      (a, bs) <- Map.toList timesArgs.powerArgs
      guard $ bs /= Seq.singleton 0
      pure $ normalizePower a $ normalizePlus bs
    allTerms =
      (if numericTerm == 1 then id else (numericTerm :<|)) $
      powerTerms

---------- Power ----------

normalizePower :: Expr -> Expr -> Expr
normalizePower a b = case (a, b) of
  (_, 0)                              -> 1
  (_, 1)                              -> a
  (ExprNumeric na, ExprNumeric nb)    -> numericPower na nb
  (Times :@ exprs, i@(ExprInteger _)) -> Times :@ (fmap (flip normalizePower i) exprs)
  (_, _)                              -> Expr.binary Power a b

-- TODO: Detect exact rational powers, e.g. Sqrt[12] -> 2*Sqrt[3]
numericPower :: Numeric -> Numeric -> Expr
numericPower nx ny = case (nx, ny) of
  (NInteger  x, NInteger  y)
    | y >= 0    -> fromInteger $ x^y
    | otherwise -> fromRational $ toRational x^^y
  (NInteger  x, NRational y) -> Expr.binary Power (ExprInteger x) (ExprRational y)
  (NInteger  x, NReal     y) -> toExpr $ realToFrac x ** y
  (NRational x, NInteger  y) -> fromRational $ x^^y
  (NRational x, NRational y) -> Expr.binary Power (ExprRational x) (ExprRational y)
  (NRational x, NReal     y) -> toExpr $ realToFrac x ** y
  (NReal     x, NInteger  y) -> toExpr $ x ** realToFrac y
  (NReal     x, NRational y) -> toExpr $ x ** realToFrac y
  (NReal     x, NReal     y) -> toExpr $ x ** y

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

---------- Function ----------

$(declareBuiltin ''Expr 'fromString "Function" "Function")
$(declareBuiltin ''Expr 'fromString "Let" "Let")
$(declareBuiltin ''Expr 'fromString "Module" "Module")

-- | Detects any new variables introduced by the given
-- expression. This is used to avoid overwriting shadowing variables
-- in constructs like Function, Let, and Module.
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
  _ -> Empty

-- | Replace the Symbols in the given Expr with their corresponding
-- Bindings in 'substSet', allowing new local variables introduced in
-- sub-expressions to shadow the given substitutions.
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

-- | A datatype that matches a single expression e or a list of
-- expressions {e1,...,en}, such that the expressions can all be
-- mapped to the type 'a'.
newtype ListOrSolo a = MkListOrSolo (Seq a)
instance FromExpr a => FromExpr (ListOrSolo a) where
  fromExpr = fmap MkListOrSolo . \case
    List :@ es -> mapM fromExpr es
    e          -> fmap pure $ fromExpr e

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

moduleDef :: ListOrSolo Symbol -> Expr -> EvalM (Maybe Expr)
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

---------- ReplaceAll ----------

$(declareBuiltin ''Expr 'fromString "RuleDelayed" "RuleDelayed")

newtype ARuleDelayed = MkRuleDelayed Rule
instance FromExpr ARuleDelayed where
  fromExpr = \case
    RuleDelayed :@ Pair lhs rhs -> Just $ MkRuleDelayed $ PatRule (patFromExpr lhs) rhs
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
replaceAll :: Expr -> ListOrSolo ARuleDelayed -> EvalM Expr
replaceAll e (MkListOrSolo rules) = go e
  where
    -- Repeatedly try rules in the given Sequence until one of them
    -- works
    tryRules _ Empty = pure Nothing
    tryRules expr (MkRuleDelayed r :<| rs) = tryApplyRule r expr >>= \case
      result@(Just _) -> pure result
      Nothing         -> tryRules expr rs

    go expr = tryRules expr rules >>= \case
      Just result -> pure result
      -- If none of the rules work, go for the head and children
      Nothing -> case expr of
        h :@ cs -> do
          h' <- go h
          cs' <- traverse go cs
          pure $ h' :@ cs'
        _       -> pure expr

---------- ReplaceRepeated ----------

replaceRepeated :: Expr -> ListOrSolo ARuleDelayed -> EvalM Expr
replaceRepeated expr rules = go expr
  where
    go currentExpr = do
      newExpr <- replaceAll currentExpr rules >>= eval
      if newExpr /= currentExpr
        then go newExpr
        else pure newExpr

---------- NumericFunctionQ ----------

numericFunctionQ :: Symbol -> EvalM Bool
numericFunctionQ sym = do
  ctx <- getContext
  pure (lookupAttributes sym ctx).numericFunction

-- ========== Building Contexts ========== --

decls :: MonadState Context m => [Text] -> m ()
decls = mapM_ (addDecl . parseDecl)

parseDecl :: Text -> Decl
parseDecl declText =
  maybe (error $ "Couldn't parse declaration: " ++ show declText) id $ do
  declExpr <- parseExprText declText
  case declExpr of
    SetDelayed :@ (Pair (ExprSymbol sym) rhs) -> Just $ OwnValue sym rhs
    SetDelayed :@ (Pair (TagSetDelayed :@ (Pair (ExprSymbol sym) patExpr)) rhs) ->
      Just $ UpValue sym (PatRule (patFromExpr patExpr) rhs)
    SetDelayed :@ (Pair patExpr rhs) ->
      let pat = patFromExpr patExpr
      in case patRootSymbol pat of
        Just sym -> Just $ DownValue sym (PatRule pat rhs)
        Nothing  -> error $ "Pattern has no root symbol: " ++ show pat
    _ -> Nothing

function :: (ToBuiltin f, MonadState Context m) => Symbol -> f -> m ()
function sym f = addDecl $ builtinDecl sym f

addStdLib :: MonadState Context m => m ()
addStdLib = do
  addDecl functionDecl
  modifyAttributes "Function" (setHoldType HoldAll)

  function "Let" letDef
  modifyAttributes "Let" (setHoldType HoldAll)

  function "Module" moduleDef
  modifyAttributes "Module" (setHoldType HoldAll)

  modifyAttributes "If" (setHoldType HoldRest)
  decls
    [ "If[True,  x_, _ ] := x"
    , "If[False, _,  y_] := y"
    ]

  function "And" normalizeAnd
  modifyAttributes "And" setFlat

  function "Or" normalizeOr
  modifyAttributes "Or" setFlat

  function "Equal" equalDef
  function "Greater" greaterDef
  function "Less" lessDef
  function "GreaterEqual" greaterEqualDef
  function "LessEqual" lessEqualDef

  function "ReplaceAll" replaceAll
  function "ReplaceRepeated" replaceRepeated
  modifyAttributes "RuleDelayed" (setHoldType HoldAll)

  function "Map" mapDef

  function "Plus" normalizePlus
  modifyAttributes "Plus" (setFlat . setOrderless)

  function "Times" normalizeTimes
  modifyAttributes "Times" (setFlat . setOrderless)

  function "Power" normalizePower

  function "SameQ" sameQ
  function "OrderedQ" orderedQ

  function "NumericFunctionQ" numericFunctionQ
  sequence_ $ do
    numFn <- builtinNumericFunctions
    pure $ modifyAttributes numFn setNumericFunction

  function "NumericQ" $ \(_ :: Numeric) -> True
  decls
    [ "NumericQ[Pi] := True"
    , "NumericQ[E] := True"
    , "NumericQ[f_[xs___]] := NumericFunctionQ[f] && And @@ Map[NumericQ, {xs}]"
    , "NumericQ[_] := False"
    ]

  decls
    [ "Apply[h_,hp_[xs___]] := h[xs]"
    , "UnsameQ[xs___] := Not[SameQ[xs]]"
    , "Not[True]    := False"
    , "Not[False]   := True"
    , "Not[Not[x_]] := x"
    ]

  function "MultinomialPowerExpand" multinomialPowerExpand
  decls
    [ "Expand[a_ * b_Plus] := (Expand[a*#]&) /@ b"
    , "Expand[b_Plus] := Expand /@ b"
    , "Expand[Power[b_Plus, c_Integer]]      /; c >= 0 := Expand /@ MultinomialPowerExpand[b,c]"
    , "Expand[a_ * Power[b_Plus, c_Integer]] /; c >= 0 := (Expand[a*#]&) /@ MultinomialPowerExpand[b,c]"
    , "Expand[expr_] := expr"
    ]

addVectorCalculus :: MonadState Context m => m ()
addVectorCalculus = do
  modifyAttributes "CenterDot" setOrderless
  decls
    [ "ScalarQ[x_]/;NumericQ[x] := True"
    , "ScalarQ[_] := False"
    , "CenterDot[0, v_] := 0"
    , "CenterDot[x_Plus,y_] := (CenterDot[#,y]&) /@ x"
    , "CenterDot[a_*u_, v_] /; ScalarQ[a] := a*CenterDot[u,v]"
    , "CenterDot /: Times[CenterDot[u_,basis[i_]], CenterDot[v_,basis[i_]], xs___] := Times[CenterDot[u,v],xs]"
    , "CenterDot /: Power[CenterDot[u_,basis[i_]],2] := CenterDot[u,u]"
    , "CenterDot[basis[i_],basis[i_]] := dim"
    , "deriv[u_,x_][expr_Plus] := deriv[u,x] /@ expr"
    , "deriv[u_,x_][a_*b_] := deriv[u,x][a]*b + a*deriv[u,x][b]"
    , "deriv[u_,x_][a_^b_] := b*deriv[u,x][a]*a^(b-1)"
    , "deriv[u_,x_][CenterDot[x_,x_]] := 2*CenterDot[u,x]"
    , "deriv[u_,x_][CenterDot[x_,y_]] := CenterDot[u,y]"
    , "deriv[_,_][n_] /; NumericQ[n] := 0"
    , "deriv[_,_][dim] := 0"
    , "laplacian[x_][expr_] := Module[{i}, deriv[basis[i],x][deriv[basis[i],x][expr]]]"
    ]


myContext :: Context
myContext = createContext $ do
  addStdLib
  addVectorCalculus
  decls
    [ "square[x_] := x*x"
    , "fib[0] := 0"
    , "fib[1] := 1"
    , "fib[n_] := fib[n-1] + fib[n-2]"
    , "z := 9"
    , "buz := False"
    , "forz[Times[x_,y_]] := biz[x,y]"
    -- , "main := forz[Times[a,b,c]]"
    -- , "main := a*c*b"
    --, "main := NumericQ/@{Sin[12], Pi, E, 12, 0.5}"
    -- , "main := (1+#&) /@ baz[1,2,3] + (#1*#2&)[2,3]"
    -- , "main := CenterDot[2*x+y,u+v]"
    --, "main := CenterDot[x,basis[i]]*CenterDot[y,basis[i]] + CenterDot[2*x,basis[i]]^2 + CenterDot[basis[j],basis[j]]^2 /. dim :> 3"
    -- , "main := deriv[u,x][CenterDot[x,y]+2*CenterDot[x,x]^3]"
    -- , "main := ScalarQ[1+3^(1/2)]"
    -- , "main := CenterDot[3*x,2*y+v-v-2*y]"
    -- , "main := Function[Slot[1]*Function[1+Slot[1]]][y]"
    --, "main := Let[{z=12}, z+1]"
    -- , "main := f[f[x],f[y]] /. f :> g"
    -- , "main := f[f[x],f[y]] //. { f :> g, g :> h, h :> j }"
    --, "main := Let[{x=9},{Module[x,x+1], Module[{x,y},x*y]}]"
    --, "main := Function[x, x+2][9]"
    --, "main := Let[x=2, Function[x, Function[x,x+9]][12][x]]"
    -- , "bar := Function[x,x+2]"
    -- , "main := bar /. x:>10"
    , "main := Expand[laplacian[x][CenterDot[x,x]^((2-dim)/2)]]"
    --, "main := Expand[(x+2)*(x+3)*(x^2 + 9*x+12)]"
    -- , "main := Expand[(x+2)^3*(1+2*y+3*x)]"
    --, "main := x^3 /. { Power[a_,b_Integer] /; (b>=0) :>FOO[a,b] }"
    ]

