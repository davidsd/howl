{-# LANGUAGE LambdaCase #-}

-- | Numeric types and conversions used by Howl expressions.
--
-- This module defines the 'Numeric' datatype, together with
-- conversions to and from common Haskell numeric types and MPFR-based
-- arbitrary-precision floating-point numbers.
module Howl.Expr.Numeric
  ( Numeric(..)
  , FromNumeric(..)
  , BigFloat
  , BigFloatPrecision
  , fromBigFloat
  , toDouble
  , toBigFloat
  , bigFloatPrecision
  ) where

-- | A Numeric datatype that is essentially a dynamically-typed
-- number. The Num and Fractional instances automatically convert
-- to/from Integer, Rational, BigFloat, and Double as needed for
-- the computation.

import Data.Bifunctor         (second)
import Data.Ratio             (denominator, numerator)
import Howl.PPrint            (PPrint (..))
import Numeric.Rounded.Simple (Rounded)
import Numeric.Rounded.Simple qualified as Rounded

-- | An arbitrary-precision floating-point number implemented using MPFR.
type BigFloat = Rounded

-- | The precision of a 'BigFloat'.
type BigFloatPrecision = Rounded.Precision

-- | A numeric value.
data Numeric
  = -- | An integer.
    NInteger  !Integer
  | -- | A rational number.
    NRational !Rational
  | -- | A double-precision floating-point number.
    NDouble   !Double
  | -- | An MPFR floating-point number with dynamic precision.
    NBigFloat !BigFloat
  deriving (Eq)

binaryApply
  :: (Integer -> Integer -> a)
  -> (Rational -> Rational -> a)
  -> (Double -> Double -> a)
  -> (BigFloat -> BigFloat -> a)
  -> Numeric -> Numeric -> a
binaryApply fi fr fd fbf n1 n2 = case (n1, n2) of
  (NDouble x, _)             -> fd x (toDouble n2)
  (_, NDouble y)             -> fd (toDouble n1) y
  (NBigFloat x, NBigFloat y) -> fbf x y
  (NBigFloat x, _)           -> fbf x (toBigFloat (bigFloatPrecision x) n2)
  (_, NBigFloat y)           -> fbf (toBigFloat (bigFloatPrecision y) n1) y
  (NInteger x,  NInteger y ) -> fi x y
  (NRational x, NInteger y ) -> fr x (fromIntegral y)
  (NInteger x,  NRational y) -> fr (fromIntegral x) y
  (NRational x, NRational y) -> fr x y

unaryApply
  :: (Integer -> a)
  -> (Rational -> a)
  -> (Double -> a)
  -> (BigFloat -> a)
  -> Numeric -> a
unaryApply fi fr fd fbf n = case n of
  NInteger x  -> fi x
  NRational x -> fr x
  NDouble x   -> fd x
  NBigFloat x -> fbf x

instance Ord Numeric where
  compare = binaryApply compare compare compare compare

showRational :: Rational -> String
showRational r =
  show (numerator r) ++
  if denominator r == 1
  then ""
  else "/" ++ show (denominator r)

instance PPrint Numeric where
  pPrint (NInteger i)  = show i
  pPrint (NRational r) = showRational r
  pPrint (NDouble r)   = show r
  pPrint (NBigFloat r) = Rounded.show' r

instance Show Numeric where
  showsPrec p = \case
    NInteger i ->
      showParen (p > 10) $
        showString "NInteger " . showsPrec 11 i
    NRational r ->
      showParen (p > 10) $
        showString "NRational " . showsPrec 11 r
    NDouble d ->
      showParen (p > 10) $
        showString "NDouble " . showsPrec 11 d
    NBigFloat b ->
      showParen (p > 10) $
        showString "NBigFloat " . showString (Rounded.show' b)

-- | A version of compose where the first function takes two arguments
(.#) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
f .# g = \x y -> f (g x y)

instance Num Numeric where
  (+) = binaryApply (fromInteger .# (+)) (fromRational .# (+)) (fromDouble .# (+)) (fromBigFloat .# bigFloatAdd)
  (*) = binaryApply (fromInteger .# (*)) (fromRational .# (*)) (fromDouble .# (*)) (fromBigFloat .# bigFloatMul)
  negate = unaryApply (fromInteger . negate) (fromRational . negate) (fromDouble . negate) (fromBigFloat . bigFloatNegate)
  abs    = unaryApply (fromInteger . abs)    (fromRational . abs)    (fromDouble . abs)    (fromBigFloat . bigFloatAbs)
  signum = unaryApply (fromInteger . signum) (fromRational . signum) (fromDouble . signum) (fromBigFloat . bigFloatSignum)
  fromInteger = NInteger

instance Fractional Numeric where
  recip = unaryApply (fromRational . recip . toRational) (fromRational . recip) (fromDouble . recip) (fromBigFloat . bigFloatRecip)
  fromRational r
    | denominator r == 1 = NInteger (numerator r)
    | otherwise          = NRational r

instance Real Numeric where
  toRational = unaryApply toRational id toRational (Rounded.toRational' Rounded.TowardNearest)

instance RealFrac Numeric where
  properFraction =
    unaryApply
    (\i -> (fromIntegral i, 0))
    (second fromRational . properFraction)
    (second fromDouble . properFraction)
    (second fromBigFloat . Rounded.properFraction_)

instance Enum Numeric where
  toEnum = fromIntegral
  fromEnum = fromInteger . truncate
  enumFromThenTo x1 x2 xFinal = go x1
    where
      dx = x2 - x1
      go x
        | x <= xFinal = x : go (x+dx)
        | otherwise = []

-- | A class for extracting Haskell numeric values from a 'Numeric'.
class FromNumeric a where
  fromNumeric :: Numeric -> Maybe a

instance FromNumeric Integer where
  fromNumeric (NInteger i) = Just i
  fromNumeric (NRational r)
    | denominator r == 1 = Just (numerator r)
    | otherwise          = Nothing
  fromNumeric _ = Nothing

instance FromNumeric Int where
  fromNumeric = fmap fromIntegral . fromNumeric @Integer

instance FromNumeric Rational where
  fromNumeric (NInteger i)  = Just $ fromIntegral i
  fromNumeric (NRational r) = Just r
  fromNumeric _             = Nothing

-- TODO: FromNumeric for Rounded with type-level precision!

instance FromNumeric Double where
  fromNumeric = Just . toDouble

fromDouble :: Double -> Numeric
fromDouble = NDouble

-- | Convert a 'BigFloat' to a 'Numeric'.
fromBigFloat :: BigFloat -> Numeric
fromBigFloat = NBigFloat

defaultRounding :: Rounded.RoundingMode
defaultRounding = Rounded.TowardNearest

-- | Get the precision of a 'BigFloat'.
bigFloatPrecision :: BigFloat -> BigFloatPrecision
bigFloatPrecision = Rounded.precision

{-# INLINE toDouble #-}
-- | Convert a 'Numeric' to a 'Double'.
toDouble :: Numeric -> Double
toDouble = \case
  NInteger x  -> fromIntegral x
  NRational x -> realToFrac x
  NDouble x   -> x
  NBigFloat x -> Rounded.toDouble defaultRounding x

{-# INLINE toBigFloat #-}
-- | Convert a 'Numeric' to a 'BigFloat' with the given precision.
toBigFloat :: BigFloatPrecision -> Numeric -> BigFloat
toBigFloat p = \case
  NInteger x  -> Rounded.fromInteger' defaultRounding p x
  NRational x -> Rounded.fromRational' defaultRounding p x
  NDouble x   -> Rounded.fromDouble defaultRounding p x
  NBigFloat x -> x

bigFloatBinary
  :: (Rounded.RoundingMode -> BigFloatPrecision -> BigFloat -> BigFloat -> BigFloat)
  -> BigFloat
  -> BigFloat
  -> BigFloat
bigFloatBinary f x y =
  let p = min (bigFloatPrecision x) (bigFloatPrecision y)
  in f defaultRounding p x y

bigFloatUnary
  :: (Rounded.RoundingMode -> BigFloatPrecision -> BigFloat -> BigFloat)
  -> BigFloat
  -> BigFloat
bigFloatUnary f x = f defaultRounding (bigFloatPrecision x) x

bigFloatAdd :: BigFloat -> BigFloat -> BigFloat
bigFloatAdd = bigFloatBinary Rounded.add_

bigFloatMul :: BigFloat -> BigFloat -> BigFloat
bigFloatMul = bigFloatBinary Rounded.mul_

bigFloatNegate :: BigFloat -> BigFloat
bigFloatNegate = bigFloatUnary Rounded.negate_

bigFloatAbs :: BigFloat -> BigFloat
bigFloatAbs = bigFloatUnary Rounded.abs_

bigFloatRecip :: BigFloat -> BigFloat
bigFloatRecip x =
  let p = bigFloatPrecision x
      one = Rounded.fromInteger' defaultRounding p 1
  in bigFloatBinary Rounded.div_ one x

bigFloatSignum :: BigFloat -> BigFloat
bigFloatSignum x =
  let p = bigFloatPrecision x
      zero = Rounded.fromInteger' defaultRounding p 0
      one = Rounded.fromInteger' defaultRounding p 1
      negOne = Rounded.fromInteger' defaultRounding p (-1)
  in if x == zero then zero else if x > zero then one else negOne
