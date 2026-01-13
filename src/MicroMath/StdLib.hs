{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}

module MicroMath.StdLib where

import Control.Monad     (guard)
import Data.Foldable     qualified as Foldable
import Data.Map.Strict   (Map)
import Data.Map.Strict   qualified as Map
import Data.Sequence     (Seq, pattern (:<|), pattern Empty)
import Data.Sequence     qualified as Seq
import Data.Text         (Text)
import Debug.Trace       qualified as Debug
import MicroMath.Context (Attributes (..), Context (..), ContextM,
                          HoldType (..), Rule (..), addDownValue, addPatRule,
                          addUpValue, createContext, emptyAttributes,
                          functionRule, setAttributes)
import MicroMath.Eval    (Substitution (..), SubstitutionSet,
                          applySubstitutions, emptySubstitutionSet,
                          insertSubstitution)
import MicroMath.Expr    hiding (False, True)
import MicroMath.Expr    qualified as Expr
import MicroMath.Parser  (parseExprText)
import MicroMath.Pat     (Pat, setDelayedFromExpr)
import MicroMath.Symbol  (Symbol)
import MicroMath.Util    (pattern Pair, pattern Solo)

withHead :: Expr -> (Seq Expr -> Expr) -> (Expr -> Maybe Expr)
withHead h f = withHeadMaybe h (Just . f)

withHeadMaybe :: Expr -> (Seq Expr -> Maybe Expr) -> (Expr -> Maybe Expr)
withHeadMaybe h f = \case
  h' :@ args | h == h' -> f args
  _                    -> Nothing

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
    _      -> Plus :@ (Seq.unstableSort $ flattenWithHead Plus allTerms)
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

builtinPlusRule :: Rule
builtinPlusRule = functionRule $ withHead Plus normalizePlus

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
    _      -> Times :@ (Seq.unstableSort $ flattenWithHead Times allTerms)
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

builtinTimesRule :: Rule
builtinTimesRule = functionRule $ withHead Times normalizeTimes

normalizePower :: Expr -> Expr -> Expr
normalizePower a b = case (a, b) of
  (_, 0)                           -> 1
  (_, 1)                           -> a
  (ExprNumeric na, ExprNumeric nb) -> numericPower na nb
  (_, _)                           -> Expr.binary Power a b

-- TODO: Detect exact rational powers
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

builtinPowerRule :: Rule
builtinPowerRule = functionRule $ withHeadMaybe Power $ \case
  Pair x y -> Just $ normalizePower x y
  _        -> Nothing

-- | TODO: Implement a general predicate
orderedQRule :: Rule
orderedQRule = functionRule $ withHeadMaybe OrderedQ $ \case
  Solo (_ :@ Empty)      -> Just $ Expr.True
  Solo (_ :@ (x :<| xs)) -> Just $ go x xs
  _ -> Nothing
  where
    go _ Empty = Expr.True
    go prev (y :<| ys)
      | prev <= y = go y ys
      | otherwise = Expr.False

sameQRule :: Rule
sameQRule = functionRule $ withHead SameQ $ \case
  Empty      -> Expr.True
  x :<| rest -> fromBool $ all (== x) rest

andRule :: Rule
andRule = functionRule $ withHead And $ \args ->
  case filterBools args of
    Nothing       -> Expr.False
    Just Empty    -> Expr.True
    Just (Solo x) -> x
    Just xs       -> And :@ xs
  where
    filterBools Empty                = Just Empty
    filterBools (Expr.False :<| _)   = Nothing
    filterBools (Expr.True :<| rest) = filterBools rest
    filterBools (x :<| xs)           = fmap (x :<|) (filterBools xs)

orRule :: Rule
orRule = functionRule $ withHead Or $ \args ->
  case filterBools args of
    Nothing       -> Expr.True
    Just Empty    -> Expr.False
    Just (Solo x) -> x
    Just xs       -> Or :@ xs
  where
    filterBools Empty                 = Just Empty
    filterBools (Expr.True :<| _)     = Nothing
    filterBools (Expr.False :<| rest) = filterBools rest
    filterBools (x :<| xs)            = fmap (x :<|) (filterBools xs)

-- | Function[x,body][y] is implemented by performing the symbolic
-- substitution body /. x->y *before evaluation*. As a consequence,
-- any occurences of x inside body will be substituted with y,
-- regardless of whether the symbol x is bound to anything outside the
-- scope of the Function. In other words, function variables exhibit
-- shadowing.
lambdaRule :: Rule
lambdaRule = functionRule $ \case
  (Function :@ Solo body) :@ _ -> Just body
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

-- | We transform Let as follows:
--
-- Let[{x=x0}, expr]      -> Function[x,expr][x0]
-- Let[{x=x0,y=y0}, expr] -> Function[x,Function[y,expr][y0]][x0]
-- etc.
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
letRule :: Rule
letRule = functionRule $ withHeadMaybe Let $ \case
  Pair (List :@ bindings) body
    | Just substs <- mapM toSubst_maybe bindings
      -> Just $ Foldable.foldr funApp body substs
  _ -> Nothing
  where
    toSubst_maybe (Set :@ Pair (ExprSymbol x) y) = Just (x, y)
    toSubst_maybe _                              = Nothing

    funApp (x, x0) expr = Function :@ Pair (ExprSymbol x) expr :@ Solo x0

downValues :: [(Symbol, Rule)] -> ContextM ()
downValues = mapM_ (uncurry addDownValue)

upValues :: [(Symbol, Rule)] -> ContextM ()
upValues = mapM_ (uncurry addUpValue)

decls :: [Text] -> ContextM ()
decls = mapM_ (uncurry addPatRule . parseDecl)

addStdLib :: ContextM ()
addStdLib = do
  setAttributes "Function" $ emptyAttributes { holdType = Just HoldAll }
  setAttributes "Let"      $ emptyAttributes { holdType = Just HoldAll }
  setAttributes "If"       $ emptyAttributes { holdType = Just HoldRest }
  setAttributes "Plus"     $ emptyAttributes { flat = True, orderless = True }
  setAttributes "Times"    $ emptyAttributes { flat = True, orderless = True }
  setAttributes "And"      $ emptyAttributes { flat = True }
  setAttributes "Or"       $ emptyAttributes { flat = True }
  downValues
    [ ("Function", lambdaRule)
    , ("Let",      letRule)
    , ("Plus",     builtinPlusRule)
    , ("Times",    builtinTimesRule)
    , ("Power",    builtinPowerRule)
    , ("SameQ",    sameQRule)
    , ("OrderedQ", orderedQRule)
    , ("And",      andRule)
    , ("Or",       orRule)
    ]
  decls
    [ "UnsameQ[xs___] := Not[SameQ[xs]]"
    , "Not[True]    := False"
    , "Not[False]   := True"
    , "Not[Not[x_]] := x"
    , "If[True,  x_, _ ] := x"
    , "If[False, _,  y_] := y"
    ]

parseDecl :: Text -> (Pat, Expr)
parseDecl declText =
  maybe (error $ "Couldn't parse declaration: " ++ show declText) id $ do
  decl <- parseExprText declText
  setDelayedFromExpr decl

myContext :: Context
myContext = createContext $ do
  addStdLib
  decls
    [ "square[x_] := x*x"
    , "fib[0] := 0"
    , "fib[1] := 1"
    , "fib[n_] := fib[n-1] + fib[n-2]"
    , "z := 9"
    , "buz := False"
    , "main := If[SameQ[x,x], fib[3], fib[4]]"
    ]

