module MicroMath.Expr.Numeric where

-- | A Numeric datatype that is essentially a dynamically-typed
-- number. The Num and Fractional instances automatically convert
-- to/from Integer, Rational, Double as needed for the computation.

import Data.Ratio (numerator, denominator)

data Numeric
  = NInteger  !Integer
  | NRational !Rational
  | NReal     !Double
  deriving (Eq)

binaryApply
  :: (Integer -> Integer -> a)
  -> (Rational -> Rational -> a)
  -> (Double -> Double -> a)
  -> Numeric -> Numeric -> a
binaryApply fi fr fd n1 n2 = case (n1, n2) of
  (NInteger x,  NInteger y ) -> fi x y
  (NRational x, NInteger y ) -> fr x (fromIntegral y)
  (NReal x,     NInteger y ) -> fd x (realToFrac y)
  (NInteger x,  NRational y) -> fr (fromIntegral x) y
  (NRational x, NRational y) -> fr x y
  (NReal x,     NRational y) -> fd x (realToFrac y)
  (NInteger x,  NReal y    ) -> fd (realToFrac x) y
  (NRational x, NReal y    ) -> fd (realToFrac x) y
  (NReal x,     NReal y    ) -> fd x y

unaryApply
  :: (Integer -> a)
  -> (Rational -> a)
  -> (Double -> a)
  -> Numeric -> a
unaryApply fi fr fd n = case n of
  NInteger x  -> fi x
  NRational x -> fr x
  NReal x     -> fd x

instance Ord Numeric where
  compare = binaryApply compare compare compare

showRational :: Rational -> String
showRational r =
  show (numerator r) ++
  if denominator r == 1
  then ""
  else "/" ++ show (denominator r)

instance Show Numeric where
  show (NInteger i)  = show i
  show (NRational r) = showRational r
  show (NReal r)     = show r

-- | A version of compose where the first function takes two arguments
(.#) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
f .# g = \x y -> f (g x y)

instance Num Numeric where
  (+) = binaryApply (fromInteger .# (+)) (fromRational .# (+)) (fromReal .# (+))
  (*) = binaryApply (fromInteger .# (*)) (fromRational .# (*)) (fromReal .# (*))
  negate = unaryApply (fromInteger . negate) (fromRational . negate) (fromReal . negate)
  abs    = unaryApply (fromInteger . abs)    (fromRational . abs)    (fromReal . abs)
  signum = unaryApply (fromInteger . signum) (fromRational . signum) (fromReal . signum)
  fromInteger = NInteger

instance Fractional Numeric where
  recip = unaryApply (fromRational . recip . toRational) (fromRational . recip) (fromReal . recip)
  fromRational r
    | denominator r == 1 = NInteger (numerator r)
    | otherwise          = NRational r

fromReal :: Double -> Numeric
fromReal = NReal
