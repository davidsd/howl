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

import Control.Monad          (guard, void)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable          qualified as Foldable
import Data.Map.Strict        (Map)
import Data.Map.Strict        qualified as Map
import Data.Sequence          (Seq, pattern (:<|), pattern (:|>), pattern Empty)
import Data.Sequence          qualified as Seq
import Data.String            (fromString)
import Data.Text              (Text)
import Data.Text.IO           qualified as Text
import Debug.Trace            qualified as Debug
import Math.Combinat          (binomial, multinomial)
import MicroMath.Context      (Attributes (..), Decl (..), Eval (..),
                               HoldType (..), Rule (..), addDecl, clear,
                               clearAll, lookupAttributes, lookupSymbolRecord,
                               modifyAttributes, newModuleSymbol, setFlat,
                               setHoldType, setNumericFunction, setOrderless)
import MicroMath.Eval         (Substitution (..), SubstitutionSet,
                               emptySubstitutionSet, eval, insertSubstitution,
                               insertSubstitutions, lookupBinding,
                               removeBindings, tryApplyRule)
import MicroMath.Expr         (Expr (..), FromExpr (..), Numeric (..),
                               ToExpr (..), builtinNumericFunctions,
                               pattern (:@), pattern And, pattern ExprInteger,
                               pattern ExprNumeric, pattern ExprRational,
                               pattern ExprView, pattern List, pattern Null,
                               pattern Or, pattern Plus, pattern Power,
                               pattern Set, pattern Slot, pattern TagSetDelayed,
                               pattern Times)
import MicroMath.Expr         qualified as Expr
import MicroMath.Expr.TH      (declareBuiltin)
import MicroMath.Parser       (parseCompoundExpressionText)
import MicroMath.Pat          (Pat, patFromExpr, patRootSymbol)
import MicroMath.Symbol       (Symbol)
import MicroMath.ToBuiltin    (ToBuiltin (..), builtinDecl)
import MicroMath.Util         (pattern Pair, pattern Solo)

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
normalizeTimes initialArgs =
  case allTerms of
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
  (NInteger  x, NInteger  y)
    | y >= 0    -> ExprInteger $ x^y
    | otherwise -> ExprRational $ toRational x^^y
  (NInteger  x, NRational y) -> Expr.binary Power (ExprInteger x) (ExprRational y)
  (NInteger  x, NReal     y) -> toExpr $ realToFrac x ** y
  (NRational x, NInteger  y) -> ExprRational $ x^^y
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
replaceAll :: Expr -> ListOrSolo ARuleDelayed -> Eval Expr
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

replaceRepeated :: Expr -> ListOrSolo ARuleDelayed -> Eval Expr
replaceRepeated expr rules = go expr
  where
    go currentExpr = do
      newExpr <- replaceAll currentExpr rules >>= eval
      if newExpr /= currentExpr
        then go newExpr
        else pure newExpr

---------- NumericFunctionQ ----------

numericFunctionQ :: Symbol -> Eval Bool
numericFunctionQ = fmap (.numericFunction) . lookupAttributes

---------- SetDelayed and Set ----------

data LHS
  = LHSSymbol Symbol
  | LHSPat Symbol Pat
  | LHSTaggedPat Symbol Pat
  deriving (Show)

-- | TODO: Add UpSet
instance FromExpr LHS where
  fromExpr = \case
    ExprSymbol sym                                   -> Just $ LHSSymbol sym
    TagSetDelayed :@ (Pair (ExprSymbol sym) patExpr) -> Just $ LHSTaggedPat sym (patFromExpr patExpr)
    patExpr ->
      let pat = patFromExpr patExpr
      in case patRootSymbol pat of
        Just rootSym -> Just (LHSPat rootSym pat)
        Nothing      -> Nothing

setPairToDecl :: LHS -> Expr -> Decl
setPairToDecl lhs rhs = case lhs of
  LHSSymbol sym        -> OwnValue sym rhs
  LHSPat sym pat       -> DownValue sym (PatRule pat rhs)
  LHSTaggedPat sym pat -> UpValue sym (PatRule pat rhs)

-- | There are two differences between SetDelayed and Set. Firstly,
-- SetDelayed has attribute HoldAll, so that the rhs is unevaluated
-- when the rule is added to the Context. By contrast, Set has
-- attribute HoldFirst, so that the rhs (its second argument) is
-- evaluated before the rule is added to the Context. Secondly, 'Set'
-- returns the rhs, which has already been evaluated.
--
setDef :: LHS -> Expr -> Eval Expr
setDef lhs rhs = do
  addDecl $ setPairToDecl lhs rhs
  pure rhs

setDelayedDef :: LHS -> Expr -> Eval ()
setDelayedDef lhs rhs = addDecl $ setPairToDecl lhs rhs

---------- CompoundExpression ----------

-- | Since we don't give CompoundExpression any attributes, all of its
-- arguments will be pre-evaluated by the evaluator. All we have to do
-- here is return the final one.
compoundExpression :: Seq Expr -> Expr
compoundExpression = \case
  Empty       -> Null
  _ :|> final -> final

---------- Get ----------

get :: FilePath -> Eval Expr
get path = do
  contents <- liftIO $ Text.readFile path
  case parseCompoundExpressionText path contents of
    Left err   -> logMsg err
    Right expr -> pure expr

logMsg :: String -> Eval Expr
logMsg msg = liftIO (putStrLn msg) >> pure Expr.Null

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

setAttributes :: Symbol -> (ListOrSolo AttrModifier) -> Eval ()
setAttributes sym (MkListOrSolo attrs) =
  mapM_ (modifyAttributes sym . (.getModifier)) attrs

---------- Help ----------

help :: Symbol -> Eval ()
help sym = do
  maybeRecord <- lookupSymbolRecord sym
  liftIO $ putStrLn $ show maybeRecord

-- ========== Building Contexts ========== --

run :: Text -> Eval Expr
run input = case parseCompoundExpressionText "" input of
  Left err   -> logMsg err
  Right expr -> eval expr

run_ :: Text -> Eval ()
run_ = void . run

def :: ToBuiltin f => Symbol -> f -> Eval ()
def sym f = addDecl $ builtinDecl sym f

defStdLib :: Eval ()
defStdLib = do
  modifyAttributes "Set" (setHoldType HoldFirst)
  def "Set" setDef

  modifyAttributes "SetDelayed" (setHoldType HoldAll)
  def "SetDelayed" setDelayedDef

  def "CompoundExpression" compoundExpression
  def "Get" get
  def "SetAttributes" setAttributes
  def "Clear" clear
  def "ClearAll" clearAll
  def "Help" help

  addDecl functionDecl
  modifyAttributes "Function" (setHoldType HoldAll)

  def "Let" letDef
  modifyAttributes "Let" (setHoldType HoldAll)

  def "Module" moduleDef
  modifyAttributes "Module" (setHoldType HoldAll)

  modifyAttributes "If" (setHoldType HoldRest)
  mapM_ run
    [ "If[True,  x_, _ ] := x"
    , "If[False, _,  y_] := y"
    ]

  def "And" normalizeAnd
  modifyAttributes "And" setFlat

  def "Or" normalizeOr
  modifyAttributes "Or" setFlat

  def "Equal" equalDef
  def "Greater" greaterDef
  def "Less" lessDef
  def "GreaterEqual" greaterEqualDef
  def "LessEqual" lessEqualDef

  def "ReplaceAll" replaceAll
  def "ReplaceRepeated" replaceRepeated
  modifyAttributes "RuleDelayed" (setHoldType HoldAll)

  def "Map" mapDef

  def "Plus" normalizePlus
  modifyAttributes "Plus" (setFlat . setOrderless)

  def "Times" normalizeTimes
  modifyAttributes "Times" (setFlat . setOrderless)

  def "Power" normalizePower
  def "Sqrt" $ \e -> normalizePower e (ExprRational (1/2))

  def "SameQ" sameQ
  def "OrderedQ" orderedQ

  def "NumericFunctionQ" numericFunctionQ
  sequence_ $ do
    numFn <- builtinNumericFunctions
    pure $ modifyAttributes numFn setNumericFunction

  def "NumericQ" $ \(_ :: Numeric) -> True
  mapM_ run
    [ "NumericQ[Pi] := True"
    , "NumericQ[E] := True"
    , "NumericQ[f_[xs___]] := NumericFunctionQ[f] && And @@ Map[NumericQ, {xs}]"
    , "NumericQ[_] := False"
    ]

  mapM_ run
    [ "Apply[h_,hp_[xs___]] := h[xs]"
    , "UnsameQ[xs___] := Not[SameQ[xs]]"
    , "Not[True]    := False"
    , "Not[False]   := True"
    , "Not[Not[x_]] := x"
    ]

  def "MultinomialPowerExpand" multinomialPowerExpand
  mapM_ run
    [ "Expand[a_ * b_Plus] := (Expand[a*#]&) /@ b"
    , "Expand[b_Plus] := Expand /@ b"
    , "Expand[Power[b_Plus, c_Integer]]      /; c >= 0 := Expand /@ MultinomialPowerExpand[b,c]"
    , "Expand[a_ * Power[b_Plus, c_Integer]] /; c >= 0 := (Expand[a*#]&) /@ MultinomialPowerExpand[b,c]"
    , "Expand[expr_] := expr"
    ]
