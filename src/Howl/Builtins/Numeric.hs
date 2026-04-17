{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Numeric builtin definitions.
module Howl.Builtins.Numeric
  ( addNumericBuiltins
  ) where

import Howl.Builtins.ToBuiltin (def)
import Howl.Eval.Context       (Attributes (..), Eval, lookupAttributes,
                                modifyAttributes, setNumericFunction)
import Howl.Expr               (Numeric)
import Howl.Expr.Numeric       (BigFloat, BigFloatPrecision, bigFloatPrecision)
import Howl.Symbol             (Symbol)
import Numeric.Rounded.Simple  qualified as Rounded

---------- NumericFunctionQ ----------

numericFunctionQDef :: Symbol -> Eval Bool
numericFunctionQDef = fmap (.numericFunction) . lookupAttributes

bigFloatUnaryDef
  :: (Rounded.RoundingMode -> BigFloatPrecision -> BigFloat -> BigFloat)
  -> BigFloat
  -> BigFloat
bigFloatUnaryDef f x =
  f Rounded.TowardNearest (bigFloatPrecision x) x

bigFloatBinaryDef
  :: (Rounded.RoundingMode -> BigFloatPrecision -> BigFloat -> BigFloat -> BigFloat)
  -> BigFloat
  -> BigFloat
  -> BigFloat
bigFloatBinaryDef f x y =
  let p = min (bigFloatPrecision x) (bigFloatPrecision y)
  in f Rounded.TowardNearest p x y

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

-- | Register the numeric builtins and mark the appropriate symbols as
-- numeric functions.
--
-- This currently defines:
--
-- - @NumericFunctionQ@
-- - @Abs@
-- - @AiryAi@
-- - @ArcSin@
-- - @ArcCos@
-- - @ArcTan@
-- - @ArcSinh@
-- - @ArcCosh@
-- - @ArcTanh@
-- - @ArithmeticGeometricMean@
-- - @Beta@
-- - @CubeRoot@
-- - @Cos@
-- - @Cosh@
-- - @Cot@
-- - @Coth@
-- - @Csc@
-- - @Csch@
-- - @Digamma@
-- - @ExpIntegralEi@
-- - @Erf@
-- - @Erfc@
-- - @Exp@
-- - @Exp10@
-- - @Exp2@
-- - @Expm1@
-- - @Gamma@
-- - @BesselJ0@
-- - @BesselJ1@
-- - @Li2@
-- - @Log@
-- - @Log10@
-- - @Log1p@
-- - @Log2@
-- - @Max@
-- - @Min@
-- - @Sec@
-- - @Sech@
-- - @Sin@
-- - @Sinh@
-- - @Sqrt@
-- - @Tan@
-- - @Tanh@
addNumericBuiltins :: Eval ()
addNumericBuiltins = do
  defBigFloatUnary "Abs" Rounded.abs_
  defBigFloatUnary "AiryAi" Rounded.ai_
  defDoubleUnary   "ArcSin" asin
  defBigFloatUnary "ArcSin" Rounded.asin_
  defDoubleUnary   "ArcCos" acos
  defBigFloatUnary "ArcCos" Rounded.acos_
  defDoubleUnary    "ArcTan" atan
  defDoubleBinary   "ArcTan" atan2
  defBigFloatUnary  "ArcTan" Rounded.atan_
  defBigFloatBinary "ArcTan" Rounded.atan2_
  defDoubleUnary   "ArcSinh" asinh
  defBigFloatUnary "ArcSinh" Rounded.asinh_
  defDoubleUnary   "ArcCosh" acosh
  defBigFloatUnary "ArcCosh" Rounded.acosh_
  defDoubleUnary   "ArcTanh" atanh
  defBigFloatUnary "ArcTanh" Rounded.atanh_
  defBigFloatBinary "ArithmeticGeometricMean" Rounded.agm_
  defBigFloatBinary "Beta" Rounded.beta_
  defBigFloatUnary "CubeRoot" Rounded.cbrt_
  defDoubleUnary   "Cos" cos
  defBigFloatUnary "Cos" Rounded.cos_
  defDoubleUnary   "Cosh" cosh
  defBigFloatUnary "Cosh" Rounded.cosh_
  defBigFloatUnary "Cot" Rounded.cot_
  defBigFloatUnary "Coth" Rounded.coth_
  defBigFloatUnary "Csc" Rounded.csc_
  defBigFloatUnary "Csch" Rounded.csch_
  -- MPFR digamma corresponds to PolyGamma[0, z] in the Wolfram
  -- Language, but we keep the shorter name Digamma for now.
  defBigFloatUnary "Digamma" Rounded.digamma_
  defBigFloatUnary "ExpIntegralEi" Rounded.eint_
  defBigFloatUnary "Erf" Rounded.erf_
  defBigFloatUnary "Erfc" Rounded.erfc_
  defDoubleUnary   "Exp" exp
  defBigFloatUnary "Exp" Rounded.exp_
  -- There isn't a dedicated Wolfram Language symbol for 10^x, so we
  -- keep Exp10 for now.
  defBigFloatUnary "Exp10" Rounded.exp10_
  -- There isn't a dedicated Wolfram Language symbol for 2^x, so we
  -- keep Exp2 for now.
  defBigFloatUnary "Exp2" Rounded.exp2_
  -- There isn't a dedicated Wolfram Language symbol for Exp[x] - 1,
  -- so we keep Expm1 for now.
  defBigFloatUnary "Expm1" Rounded.expm1_
  defBigFloatUnary  "Gamma" Rounded.gamma_
  defBigFloatBinary "Gamma" Rounded.gamma_inc_
  -- Corresponds to BesselJ[0, x] in the Wolfram Language.
  defBigFloatUnary "BesselJ0" Rounded.j0_
  -- Corresponds to BesselJ[1, x] in the Wolfram Language.
  defBigFloatUnary "BesselJ1" Rounded.j1_
  -- MPFR Li2 corresponds to PolyLog[2, z] in the Wolfram Language,
  -- but we keep the shorter name Li2 for now.
  defBigFloatUnary "Li2" Rounded.li2_
  defDoubleUnary   "Log" log
  defDoubleBinary  "Log" logBase
  defBigFloatUnary "Log" Rounded.log_
  defBigFloatUnary "Log10" Rounded.log10_
  -- There isn't a dedicated Wolfram Language symbol for Log[1 + x],
  -- so we keep Log1p for now.
  defBigFloatUnary "Log1p" Rounded.log1p_
  defBigFloatUnary "Log2" Rounded.log2_
  defBigFloatBinary "Max" Rounded.max_
  defBigFloatBinary "Min" Rounded.min_
  defBigFloatUnary "Sec" Rounded.sec_
  defBigFloatUnary "Sech" Rounded.sech_
  defDoubleUnary   "Sin" sin
  defBigFloatUnary "Sin" Rounded.sin_
  defDoubleUnary   "Sinh" sinh
  defBigFloatUnary "Sinh" Rounded.sinh_
  defDoubleUnary   "Sqrt" sqrt
  defBigFloatUnary "Sqrt" Rounded.sqrt_
  defDoubleUnary   "Tan" tan
  defBigFloatUnary "Tan" Rounded.tan_
  defDoubleUnary   "Tanh" tanh
  defBigFloatUnary "Tanh" Rounded.tanh_
  -- Corresponds to BesselY[0, x] in the Wolfram Language.
  defBigFloatUnary "BesselY0" Rounded.y0_
  -- Corresponds to BesselY[1, x] in the Wolfram Language.
  defBigFloatUnary "BesselY1" Rounded.y1_
  defBigFloatUnary "Zeta" Rounded.zeta_
  def "NumericFunctionQ" numericFunctionQDef
  def "NumericQ" $ const @_ @Numeric True
