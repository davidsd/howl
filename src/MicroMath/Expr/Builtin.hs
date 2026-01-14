{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

module MicroMath.Expr.Builtin where

import Data.String             (fromString)
import MicroMath.Expr.Internal (Expr (..))
import MicroMath.Expr.TH       (declareBuiltins)
import MicroMath.Symbol        (Symbol)
import Prelude                 hiding (False, True)
import Prelude qualified

mkExprSymbol :: String -> Expr
mkExprSymbol = ExprSymbol . fromString

-- | declareBuiltins creates bidirectional pattern synonyms Sequence,
-- List, etc. The main advantage of these over using the IsString
-- instance for Expr and writing "Sequence", "List", etc, is that for
-- the pattern synonyms we call mkSymbol at top level precisely once,
-- whereas each time our code uses the IsString instance, it needs to
-- use 'mkSymbol' which hashes the given text and looks it up in the
-- symbol table.
--
$(declareBuiltins ''Expr 'mkExprSymbol
   [ "Sequence"
   , "Function"
   , "List"
   , "Apply"
   , "Map"
   , "Plus"
   , "Subtract"
   , "Times"
   , "Divide"
   , "Abs"
   , "Sign"
   , "Power"
   , "Blank"
   , "BlankSequence"
   , "BlankNullSequence"
   , "Slot"
   , "SlotSequence"
   , "Pattern"
   , "Alternatives"
   , "Optional"
   , "Test"
   , "Association"
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
   , "And"
   , "Or"
   , "True"
   , "False"
   , "Less"
   , "Greater"
   , "LessEqual"
   , "GreaterEqual"
   , "Equal"
   , "Unequal"
   , "SameQ"
   , "UnsameQ"
   , "Rule"
   , "RuleDelayed"
   , "ReplaceAll"
   , "ReplaceRepeated"
   , "Set"
   , "SetDelayed"
   , "UpSet"
   , "UpSetDelayed"
   , "TagSetDelayed"
   ])

fromBool :: Bool -> Expr
fromBool Prelude.True  = True
fromBool Prelude.False = False

boolView :: Expr -> Maybe Bool
boolView True  = Just Prelude.True
boolView False = Just Prelude.False
boolView _     = Nothing

pattern ExprBool :: Bool -> Expr
pattern ExprBool b <- (boolView -> Just b)
  where
    ExprBool b = fromBool b

builtinNumericFunctions :: [Symbol]
builtinNumericFunctions =
 [ "Power", "Plus", "Times", "Exp", "Log", "Sin", "Cos"
 , "Tan", "ArcSin", "ArcCos", "ArcTan", "Sinh", "Cosh"
 , "Tanh", "ArcSinh", "ArcCosh", "ArcTanh"
 ]
