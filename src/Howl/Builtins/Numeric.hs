{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}

module Howl.Builtins.Numeric
  ( addNumericBuiltins
  ) where

import Data.String       (fromString)
import Howl.Eval.Context (Attributes (..), Eval, lookupAttributes,
                          modifyAttributes, setNumericFunction)
import Howl.Expr         (Expr, Numeric)
import Howl.Expr.Numeric (BigFloat, BigFloatPrecision, bigFloatPrecision,
                          fromBigFloat)
import Howl.Expr.TH      (declareBuiltins)
import Howl.Symbol       (Symbol)
import Howl.ToBuiltin    (def)
import Numeric.Rounded.Simple qualified as Rounded

---------- NumericFunctionQ ----------

numericFunctionQDef :: Symbol -> Eval Bool
numericFunctionQDef = fmap (.numericFunction) . lookupAttributes

bigFloatUnaryDef
  :: (Rounded.RoundingMode -> BigFloatPrecision -> BigFloat -> BigFloat)
  -> BigFloat
  -> Numeric
bigFloatUnaryDef f x =
  fromBigFloat $ f Rounded.TowardNearest (bigFloatPrecision x) x

bigFloatBinaryDef
  :: (Rounded.RoundingMode -> BigFloatPrecision -> BigFloat -> BigFloat -> BigFloat)
  -> BigFloat
  -> BigFloat
  -> Numeric
bigFloatBinaryDef f x y =
  let p = min (bigFloatPrecision x) (bigFloatPrecision y)
  in fromBigFloat $ f Rounded.TowardNearest p x y

defBigFloatUnary
  :: Symbol
  -> (Rounded.RoundingMode -> BigFloatPrecision -> BigFloat -> BigFloat)
  -> Eval ()
defBigFloatUnary sym f = do
  modifyAttributes sym setNumericFunction
  def sym (bigFloatUnaryDef f)

defBigFloatBinary
  :: Symbol
  -> (Rounded.RoundingMode -> BigFloatPrecision -> BigFloat -> BigFloat -> BigFloat)
  -> Eval ()
defBigFloatBinary sym f = do
  modifyAttributes sym setNumericFunction
  def sym (bigFloatBinaryDef f)

defDoubleUnary :: Symbol -> (Double -> Double) -> Eval ()
defDoubleUnary sym f = do
  modifyAttributes sym setNumericFunction
  def sym f

defDoubleBinary :: Symbol -> (Double -> Double -> Double) -> Eval ()
defDoubleBinary sym f = do
  modifyAttributes sym setNumericFunction
  def sym f

addNumericBuiltins :: Eval ()
addNumericBuiltins = do
  defBigFloatUnary "Abs" Rounded.abs_
  -- TODO: Confirm Wolfram-facing name.
  defBigFloatUnary "Ai" Rounded.ai_
  defDoubleUnary "ArcSin" asin
  defBigFloatUnary "ArcSin" Rounded.asin_
  defDoubleUnary "ArcCos" acos
  defBigFloatUnary "ArcCos" Rounded.acos_
  defDoubleUnary "ArcTan" atan
  defDoubleBinary "ArcTan" atan2
  defBigFloatUnary "ArcTan" Rounded.atan_
  defBigFloatBinary "ArcTan" Rounded.atan2_
  defDoubleUnary "ArcSinh" asinh
  defBigFloatUnary "ArcSinh" Rounded.asinh_
  defDoubleUnary "ArcCosh" acosh
  defBigFloatUnary "ArcCosh" Rounded.acosh_
  defDoubleUnary "ArcTanh" atanh
  defBigFloatUnary "ArcTanh" Rounded.atanh_
  -- TODO: Confirm Wolfram-facing name.
  defBigFloatBinary "AGM" Rounded.agm_
  defBigFloatBinary "Beta" Rounded.beta_
  -- TODO: Confirm Wolfram-facing name.
  defBigFloatUnary "Cbrt" Rounded.cbrt_
  defDoubleUnary "Cos" cos
  defBigFloatUnary "Cos" Rounded.cos_
  defDoubleUnary "Cosh" cosh
  defBigFloatUnary "Cosh" Rounded.cosh_
  -- TODO: Confirm Wolfram-facing name.
  defBigFloatBinary "CopySign" Rounded.copysign_
  defBigFloatUnary "Cot" Rounded.cot_
  defBigFloatUnary "Coth" Rounded.coth_
  defBigFloatUnary "Csc" Rounded.csc_
  defBigFloatUnary "Csch" Rounded.csch_
  -- TODO: Confirm Wolfram-facing name.
  defBigFloatBinary "Dim" Rounded.dim_
  -- TODO: Confirm Wolfram-facing name.
  defBigFloatUnary "Digamma" Rounded.digamma_
  -- TODO: Confirm Wolfram-facing name.
  defBigFloatUnary "Eint" Rounded.eint_
  defBigFloatUnary "Erf" Rounded.erf_
  defBigFloatUnary "Erfc" Rounded.erfc_
  defDoubleUnary "Exp" exp
  defBigFloatUnary "Exp" Rounded.exp_
  -- TODO: Confirm Wolfram-facing name.
  defBigFloatUnary "Exp10" Rounded.exp10_
  -- TODO: Confirm Wolfram-facing name.
  defBigFloatUnary "Exp2" Rounded.exp2_
  -- TODO: Confirm Wolfram-facing name.
  defBigFloatUnary "Expm1" Rounded.expm1_
  -- TODO: Confirm Wolfram-facing name.
  defBigFloatBinary "FMod" Rounded.fmod_
  defBigFloatUnary "Gamma" Rounded.gamma_
  defBigFloatBinary "Gamma" Rounded.gamma_inc_
  -- TODO: Confirm Wolfram-facing name.
  defBigFloatBinary "Hypot" Rounded.hypot_
  -- TODO: Confirm Wolfram-facing name.
  defBigFloatUnary "J0" Rounded.j0_
  -- TODO: Confirm Wolfram-facing name.
  defBigFloatUnary "J1" Rounded.j1_
  -- TODO: Confirm Wolfram-facing name.
  defBigFloatUnary "Li2" Rounded.li2_
  defDoubleUnary "Log" log
  defDoubleBinary "Log" logBase
  defBigFloatUnary "Log" Rounded.log_
  -- TODO: Confirm Wolfram-facing name.
  defBigFloatUnary "Log10" Rounded.log10_
  -- TODO: Confirm Wolfram-facing name.
  defBigFloatUnary "Log1p" Rounded.log1p_
  -- TODO: Confirm Wolfram-facing name.
  defBigFloatUnary "Log2" Rounded.log2_
  defBigFloatBinary "Max" Rounded.max_
  defBigFloatBinary "Min" Rounded.min_
  -- TODO: Confirm Wolfram-facing name.
  defBigFloatUnary "RecSqrt" Rounded.rec_sqrt_
  defBigFloatUnary "Sec" Rounded.sec_
  defBigFloatUnary "Sech" Rounded.sech_
  defDoubleUnary "Sin" sin
  defBigFloatUnary "Sin" Rounded.sin_
  defDoubleUnary "Sinh" sinh
  defBigFloatUnary "Sinh" Rounded.sinh_
  -- TODO: Confirm Wolfram-facing name.
  defBigFloatUnary "Sqr" Rounded.sqr_
  defDoubleUnary "Sqrt" sqrt
  defBigFloatUnary "Sqrt" Rounded.sqrt_
  defDoubleUnary "Tan" tan
  defBigFloatUnary "Tan" Rounded.tan_
  defDoubleUnary "Tanh" tanh
  defBigFloatUnary "Tanh" Rounded.tanh_
  -- TODO: Confirm Wolfram-facing name.
  defBigFloatUnary "Y0" Rounded.y0_
  -- TODO: Confirm Wolfram-facing name.
  defBigFloatUnary "Y1" Rounded.y1_
  -- TODO: Confirm Wolfram-facing name.
  defBigFloatUnary "Zeta" Rounded.zeta_
  def "NumericFunctionQ" numericFunctionQDef
  def "NumericQ" $ const @_ @Numeric True
