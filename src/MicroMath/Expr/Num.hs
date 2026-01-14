{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

module MicroMath.Expr.Num where

import Data.Ratio              (denominator, numerator)
import Data.String             (fromString)
import MicroMath.Expr.Internal (Expr (..), Literal (..), binary, unary)
import MicroMath.Expr.Syntax   (pattern Plus, pattern Power, pattern Times)
import MicroMath.Expr.TH       (declareBuiltins)
import MicroMath.Symbol        (Symbol)

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

