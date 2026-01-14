{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}

module MicroMath.StdLib where

import Control.Monad     (guard)
import Data.Foldable     qualified as Foldable
import Data.Map.Strict   (Map)
import Data.Map.Strict   qualified as Map
import Data.Sequence     (Seq, pattern (:<|), pattern Empty)
import Data.Sequence     qualified as Seq
import Data.String       (fromString)
import Data.Text         (Text)
import MicroMath.Context (Attributes (..), Context (..), ContextM, Decl (..),
                          HoldType (..), Rule (..), addDecl, createContext,
                          functionRule, lookupAttributes, modifyAttributes,
                          setFlat, setHoldType, setNumericFunction,
                          setOrderless)
import MicroMath.Eval    (Substitution (..), SubstitutionSet,
                          applySubstitutions, emptySubstitutionSet, eval,
                          insertSubstitution, tryApplyRule)
import MicroMath.Expr    (Expr (..), Numeric (..), builtinNumericFunctions,
                          fromBool, fromReal, pattern (:@), pattern And,
                          pattern ExprInteger, pattern ExprNumeric,
                          pattern ExprRational, pattern List, pattern Or,
                          pattern Plus, pattern Power, pattern Set,
                          pattern SetDelayed, pattern Slot,
                          pattern TagSetDelayed, pattern Times)
import MicroMath.Expr    qualified as Expr
import MicroMath.Expr.TH (declareBuiltin)
import MicroMath.Parser  (parseExprText)
import MicroMath.Pat     (patFromExpr, patRootSymbol)
import MicroMath.Symbol  (Symbol)
import MicroMath.Util    (pattern Pair, pattern Solo)

withHead :: Expr -> (Seq Expr -> Expr) -> (Expr -> Maybe Expr)
withHead h f = withHeadMaybe h (Just . f)

withHeadMaybe :: Expr -> (Seq Expr -> Maybe Expr) -> (Expr -> Maybe Expr)
withHeadMaybe h f = \case
  h' :@ args | h == h' -> f args
  _                    -> Nothing

builtinFunctionMaybe :: Symbol -> (Seq Expr -> Maybe Expr) -> Decl
builtinFunctionMaybe sym = DownValue sym . functionRule . withHeadMaybe (ExprSymbol sym)

class UnpackArgs a where
  unpackArgs :: a -> (Seq Expr -> Maybe Expr)

instance UnpackArgs (Seq Expr -> Maybe Expr) where
  unpackArgs = id

instance UnpackArgs (Seq Expr -> Expr) where
  unpackArgs f = Just . f

instance UnpackArgs (Expr -> Maybe Expr) where
  unpackArgs f = \case
    Solo e -> f e
    _      -> Nothing

instance UnpackArgs (Expr -> Expr) where
  unpackArgs f = unpackArgs (Just . f)

instance UnpackArgs (Expr -> Expr -> Maybe Expr) where
  unpackArgs f = \case
    Pair e1 e2 -> f e1 e2
    _          -> Nothing

instance UnpackArgs (Expr -> Expr -> Expr) where
  unpackArgs f = unpackArgs (\e1 e2 -> Just $ f e1 e2)

instance UnpackArgs (Expr -> Expr -> Expr -> Maybe Expr) where
  unpackArgs f = \case
    (e1 :<| e2 :<| e3 :<| Empty) -> f e1 e2 e3
    _                            -> Nothing

instance UnpackArgs (Expr -> Expr -> Expr -> Expr) where
  unpackArgs f = unpackArgs (\e1 e2 e3 -> Just $ f e1 e2 e3)

function :: UnpackArgs f => Symbol -> f -> Decl
function sym f = builtinFunctionMaybe sym (unpackArgs f)

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
  | _:_ <- nums.reals = fromReal $ f $
    nums.reals <>
    map realToFrac nums.rationals <>
    map realToFrac nums.integers
  | _:_ <- nums.rationals =
      fromRational $ f $ nums.rationals <> map toRational nums.integers
  | otherwise = fromInteger $ f nums.integers

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
  (NInteger  x, NReal     y) -> fromReal $ realToFrac x ** y
  (NRational x, NInteger  y) -> fromRational $ x^^y
  (NRational x, NRational y) -> Expr.binary Power (ExprRational x) (ExprRational y)
  (NRational x, NReal     y) -> fromReal $ realToFrac x ** y
  (NReal     x, NInteger  y) -> fromReal $ x ** realToFrac y
  (NReal     x, NRational y) -> fromReal $ x ** realToFrac y
  (NReal     x, NReal     y) -> fromReal $ x ** y

---------- OrderedQ ----------

-- | OrderedQ[h[x1,...,xn]] tests whether the xi's are in the
-- canonical order defined by the Haskell ordering on expressions
-- TODO: Implement a general predicate
orderedQ :: Expr -> Maybe Expr
orderedQ = \case
  _ :@ Empty      -> Just $ Expr.True
  _ :@ (x :<| xs) -> Just $ go x xs
  _               -> Nothing
  where
    go _ Empty = Expr.True
    go prev (y :<| ys)
      | prev <= y = go y ys
      | otherwise = Expr.False

---------- SameQ ----------

sameQ :: Seq Expr -> Expr
sameQ = \case
  Empty      -> Expr.True
  x :<| rest -> fromBool $ all (== x) rest

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

---------- Function ----------

$(declareBuiltin ''Expr 'fromString "Function" "Function")

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
functionDef :: Expr -> Maybe Expr
functionDef = \case
  (Function :@ Solo body) :@ args -> Just $ replaceSlots args body
  (Function :@ Pair varsExpr body) :@ args
    | Just vars     <- symbolSeq_maybe varsExpr
    , Just bindings <- bindVars_maybe vars args
    -> Just $ applySubstitutions bindings body
  _ -> Nothing
  where
    symbolSeq_maybe :: Expr -> Maybe (Seq Symbol)
    symbolSeq_maybe (ExprSymbol x) = Just $ Solo x
    symbolSeq_maybe (List :@ xs)   = mapM toSymbol_maybe xs
    symbolSeq_maybe _              = Nothing

    bindVars_maybe :: Seq Symbol -> Seq Expr -> Maybe SubstitutionSet
    bindVars_maybe Empty      Empty      = Just emptySubstitutionSet
    bindVars_maybe (a :<| as) (b :<| bs) =
      insertSubstitution (MkSubstitution a b) =<< bindVars_maybe as bs
    bindVars_maybe _          _          = Nothing

    toSymbol_maybe :: Expr -> Maybe Symbol
    toSymbol_maybe (ExprSymbol x) = Just x
    toSymbol_maybe _              = Nothing

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

---------- Let ----------

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
-- Let[{x=9}, x] --> 9
--
-- Notice that subsequent Let bindings can refer to previously bound
-- variables:
--
-- Let[{x=9, y=x+1}, y] --> 10
--
letDef :: Expr -> Expr -> Maybe Expr
letDef = \cases
  (List :@ bindings) body
    | Just substs <- mapM toSubst_maybe bindings
      -> Just $ Foldable.foldr funApp body substs
  _ _ -> Nothing
  where
    toSubst_maybe (Set :@ Pair (ExprSymbol x) y) = Just (x, y)
    toSubst_maybe _                              = Nothing

    funApp (x, x0) expr = Function :@ Pair (ExprSymbol x) expr :@ Solo x0

---------- Map ----------

mapDef :: Expr -> Expr -> Maybe Expr
mapDef = \cases
  f (h :@ xs) -> Just $ h :@ (fmap (Expr.unary f) xs)
  _ _ -> Nothing

---------- ReplaceAll ----------

replaceAll :: Context -> Seq Rule -> Expr -> Expr
replaceAll ctx rules = go
  where
    tryRule expr rule = tryApplyRule ctx rule expr

    go expr = case Foldable.asum (fmap (tryRule expr) rules) of
      Just result -> result
      Nothing -> case expr of
        h :@ cs -> go h :@ fmap go cs
        _       -> expr

$(declareBuiltin ''Expr 'fromString "RuleDelayed" "RuleDelayed")

parseReplaceArgs :: Seq Expr -> Maybe (Expr, Seq Rule)
parseReplaceArgs = \case
  Pair expr (List :@ ruleExprs) -> (\rs -> (expr, rs))     <$> mapM parseRuleDelayed_maybe ruleExprs
  Pair expr ruleExpr            -> (\r  -> (expr, pure r)) <$> parseRuleDelayed_maybe ruleExpr
  _ -> Nothing
  where
    parseRuleDelayed_maybe = \case
      RuleDelayed :@ Pair lhs rhs -> Just $ PatRule (patFromExpr lhs) rhs
      _ -> Nothing

$(declareBuiltin ''Expr 'fromString "ReplaceAll" "ReplaceAll")

replaceAllDecl :: Decl
replaceAllDecl = DownValue "ReplaceAll" $
  BuiltinRule $ \ctx -> withHeadMaybe ReplaceAll $ \args ->
  case parseReplaceArgs args of
    Just (expr, rules) -> Just $ replaceAll ctx rules expr
    _                  -> Nothing

---------- ReplaceRepeated ----------

$(declareBuiltin ''Expr 'fromString "ReplaceRepeated" "ReplaceRepeated")

replaceRepeatedDecl :: Decl
replaceRepeatedDecl = DownValue "ReplaceRepeated" $
  BuiltinRule $ \ctx -> withHeadMaybe ReplaceRepeated $ \args ->
  case parseReplaceArgs args of
    Nothing -> Nothing
    Just (expr, rules) -> Just $ go expr
      where
        go currentExpr =
          let newExpr = eval ctx (replaceAll ctx rules currentExpr)
          in
            if newExpr /= currentExpr
            then go newExpr
            else newExpr

---------- NumericFunctionQ ----------

$(declareBuiltin ''Expr 'fromString "NumericFunctionQ" "NumericFunctionQ")

numericFunctionQDecl :: Decl
numericFunctionQDecl = DownValue "NumericFunctionQ" $
  BuiltinRule $ \ctx -> withHead NumericFunctionQ $ \case
  Solo (ExprSymbol s) -> fromBool (lookupAttributes s ctx).numericFunction
  _ -> Expr.False


-- ========== Building Contexts ========== --

decls :: [Text] -> ContextM ()
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

addStdLib :: ContextM ()
addStdLib = do
  addDecl $ function "Function" functionDef
  modifyAttributes "Function" (setHoldType HoldAll)

  addDecl $ function "Let" letDef
  modifyAttributes "Let" (setHoldType HoldAll)

  modifyAttributes "If" (setHoldType HoldRest)
  decls
    [ "If[True,  x_, _ ] := x"
    , "If[False, _,  y_] := y"
    ]

  addDecl $ function "And" normalizeAnd
  modifyAttributes "And" setFlat

  addDecl $ function "Or" normalizeOr
  modifyAttributes "Or" setFlat

  addDecl replaceAllDecl
  addDecl replaceRepeatedDecl
  modifyAttributes "RuleDelayed" (setHoldType HoldAll)

  addDecl $ function "Map" mapDef

  addDecl $ function "Plus" normalizePlus
  modifyAttributes "Plus" (setFlat . setOrderless)

  addDecl $ function "Times" normalizeTimes
  modifyAttributes "Times" (setFlat . setOrderless)

  addDecl $ function "Power" normalizePower

  addDecl $ function "SameQ" sameQ

  addDecl $ function "OrderedQ" orderedQ
  addDecl numericFunctionQDecl
  sequence_ $ do
    numFn <- builtinNumericFunctions
    pure $ modifyAttributes numFn setNumericFunction
  decls
    [ "NumericQ[_Integer] := True"
    , "NumericQ[_Rational] := True"
    , "NumericQ[_Real] := True"
    , "NumericQ[Pi] := True"
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

addVectorCalculus :: ContextM ()
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
    -- , "main := NumericQ/@{Sin[12], Pi, E, 12, 0.5}"
    -- , "main := (1+#&) /@ baz[1,2,3] + (#1*#2&)[2,3]"
    -- , "main := CenterDot[2*x+y,u+v]"
    , "main := CenterDot[x,basis[i]]*CenterDot[y,basis[i]] + CenterDot[2*x,basis[i]]^2 + CenterDot[basis[j],basis[j]]^2 /. dim :> 3"
    -- , "main := deriv[u,x][CenterDot[x,y]+2*CenterDot[x,x]^3]"
    -- , "main := ScalarQ[1+3^(1/2)]"
    -- , "main := CenterDot[3*x,2*y+v-v-2*y]"
    -- , "main := Function[Slot[1]*Function[1+Slot[1]]][y]"
    -- , "main := Let[{z=12}, z+1]"
    -- , "main := f[f[x],f[y]] /. f :> g"
    -- , "main := f[f[x],f[y]] //. { f :> g, g :> h, h :> j }"
    ]

