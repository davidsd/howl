{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}

module Howl.StdLib.Numeric
  ( addNumericBuiltins
  ) where

import Data.String       (fromString)
import Howl.Eval.Context (Attributes (..), Eval, lookupAttributes,
                          modifyAttributes, setNumericFunction)
import Howl.Expr         (Expr, Numeric)
import Howl.Expr.TH      (declareBuiltins)
import Howl.Symbol       (Symbol)
import Howl.ToBuiltin    (def)

---------- NumericFunctionQ ----------

numericFunctionQDef :: Symbol -> Eval Bool
numericFunctionQDef = fmap (.numericFunction) . lookupAttributes

-- | TODO: It is probably unnecessary to declare these all in this big
-- block. Remove them from the block as we define them one-by-one.
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

addNumericBuiltins :: Eval ()
addNumericBuiltins = do
  sequence_ $ do
    numFn <- builtinNumericFunctions
    pure $ modifyAttributes numFn setNumericFunction
  def "NumericFunctionQ" numericFunctionQDef
  def "NumericQ" $ const @_ @Numeric True
