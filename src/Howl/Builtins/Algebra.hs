{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}

module Howl.Builtins.Algebra
  ( addAlgebraBuiltins
  , normalizePlus
  ) where

import Control.Monad          (guard)
import Data.Foldable          qualified as Foldable
import Data.Map.Strict        (Map)
import Data.Map.Strict        qualified as Map
import Data.Sequence          (Seq, pattern (:<|), pattern (:|>), pattern Empty)
import Data.Sequence          qualified as Seq
import Howl.Expr              (Expr (..), FromExpr (..), Numeric (..),
                               ToExpr (..), bigFloatPrecision, pattern (:@),
                               pattern ExprInteger, pattern ExprNumeric,
                               pattern ExprRational, pattern Plus,
                               pattern Power, pattern Times, toBigFloat,
                               toDouble)
import Howl.Expr              qualified as Expr
import Howl.Eval.Context      (Eval, modifyAttributes, setFlat,
                               setNumericFunction,
                               setOrderless)
import Howl.Builtins.ToBuiltin (Variadic (..), def)
import Howl.Util              (pattern Pair, pattern Solo)
import Math.Combinat          (binomial, multinomial)
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
  (Times :@ exprs, i@(ExprInteger _)) -> Times :@ (fmap (flip normalizePower i) exprs)
  (Times :@ exprs, _) ->
    let
      (numericTerms, rest) = extractFromExprs exprs
      n = product numericTerms
      nAbs = abs n
    in
      case (n, signum n) of
        (1, _)  -> Expr.binary Power (Times :@ rest) b
        (_, 0)  -> Expr.binary Power (ExprInteger 0) b
        (-1, _) -> Expr.binary Power (Times :@ (ExprInteger (-1) :<| rest)) b
        (_, -1) ->
          Times :@ Pair
            (normalizePower (ExprNumeric nAbs) b)
            (Expr.binary Power (Times :@ (ExprInteger (-1) :<| rest)) b)
        (_, _)  ->
          Times :@ Pair
            (normalizePower (ExprNumeric n) b)
            (Expr.binary Power (Times :@ rest) b)
  (Power :@ Pair expr y@(ExprNumeric _), _) ->
    Power :@ Pair expr (normalizeTimes (Pair y b))
  (_, _) -> Expr.binary Power a b

numericPower :: Numeric -> Numeric -> Expr
numericPower nx ny = case (nx, ny) of
  (NDouble x, _) -> toExpr $ x ** toDouble ny
  (_, NDouble y) -> toExpr $ toDouble nx ** y
  (NBigFloat x, NBigFloat y) -> toExpr $ bigFloatPow x y
  (NBigFloat x, _) -> toExpr $ bigFloatPow x (toBigFloat (bigFloatPrecision x) ny)
  (_, NBigFloat y) -> toExpr $ bigFloatPow (toBigFloat (bigFloatPrecision y) nx) y
  (NInteger x, NInteger y)
    | y >= 0 -> ExprInteger $ x ^ y
    | otherwise -> ExprRational $ toRational x ^^ y
  (NInteger x, NRational y) -> Expr.binary Power (ExprInteger x) (ExprRational y)
  (NRational x, NInteger y) -> ExprRational $ x ^^ y
  (NRational x, NRational y) -> Expr.binary Power (ExprRational x) (ExprRational y)
  where
    bigFloatPow x y =
      let p = min (bigFloatPrecision x) (bigFloatPrecision y)
      in Rounded.pow_ Rounded.TowardNearest p x y

---------- MultinomialPowerExpand ----------

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
      fmap (i :<|) $ go (k - 1) (m - i)

multinomialCoeff :: Seq Integer -> Integer
multinomialCoeff xs = case xs of
  Empty    -> 1
  Solo _   -> 1
  Pair x y -> binomial (x + y) y
  _        -> multinomial (Foldable.toList xs)

addAlgebraBuiltins :: Eval ()
addAlgebraBuiltins = do
  modifyAttributes "Plus" (setNumericFunction . setFlat . setOrderless)
  def "Plus" (MkVariadic normalizePlus)

  modifyAttributes "Times" (setNumericFunction . setFlat . setOrderless)
  def "Times" (MkVariadic normalizeTimes)

  modifyAttributes "Power" setNumericFunction
  def "Power" normalizePower
  def "Sqrt" $ \e -> normalizePower e (ExprRational (1 / 2))
  def "MultinomialPowerExpand" multinomialPowerExpand
