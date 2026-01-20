{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

module MicroMath.Expr.Num where

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
