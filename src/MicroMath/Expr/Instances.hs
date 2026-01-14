{-# OPTIONS_GHC -fno-warn-orphans #-}

module MicroMath.Expr.Instances where

import MicroMath.Expr.Internal (Expr(..), binary, unary, Literal(..))
import Data.Ratio (numerator, denominator)
import MicroMath.Expr.Builtin

instance Num Expr where
  (+) = binary Plus
  (*) = binary Times
  negate x = binary Times (fromInteger (-1)) x
  abs = unary Abs
  signum = unary Sign
  fromInteger = ExprLit . LitInteger

instance Fractional Expr where
  recip x = binary Power x (fromInteger (-1))
  fromRational r = ExprLit $
    if denominator r == 1
    then LitInteger (numerator r)
    else LitRational r

instance Floating Expr where
  pi = Pi
  exp = unary Exp
  log = unary Log
  sqrt x = x ** fromRational (1/2)
  logBase = binary Log
  (**) = binary Power
  sin = unary Sin
  cos = unary Cos
  tan = unary Tan
  asin = unary ArcSin
  acos = unary ArcCos
  atan = unary ArcTan
  sinh = unary Sinh
  cosh = unary Cosh
  tanh = unary Tanh
  asinh = unary ArcSinh
  acosh = unary ArcCosh
  atanh = unary ArcTanh
