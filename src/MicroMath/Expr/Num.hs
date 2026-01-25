{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

module MicroMath.Expr.Num
  ( pattern Abs
  , pattern Sign
  , pattern Pi
  , pattern E
  , pattern Exp
  , pattern Log
  , pattern Sin
  , pattern Cos
  , pattern Tan
  , pattern ArcSin
  , pattern ArcCos
  , pattern ArcTan
  , pattern Sinh
  , pattern Cosh
  , pattern Tanh
  , pattern ArcSinh
  , pattern ArcCosh
  , pattern ArcTanh
  , builtinNumericFunctions
  ) where

import Data.String             (fromString)
import MicroMath.Expr.Internal (Expr (..))
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
