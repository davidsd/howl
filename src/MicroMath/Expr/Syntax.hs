{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

module MicroMath.Expr.Syntax where

import Data.Foldable           qualified as Foldable
import Data.Sequence           qualified as Seq
import Data.String             (fromString)
import MicroMath.Expr.Internal (Expr (..), FromExpr (..), ToExpr (..))
import MicroMath.Expr.TH       (declareBuiltins)
import Prelude                 hiding (False, True)
import Prelude qualified

-- | declareBuiltins creates bidirectional pattern synonyms Sequence,
-- List, etc. The main advantage of these over using the IsString
-- instance for Expr and writing "Sequence", "List", etc, is that for
-- the pattern synonyms we call mkSymbol at top level precisely once,
-- whereas each time our code uses the IsString instance, it needs to
-- use 'mkSymbol' which hashes the given text and looks it up in the
-- symbol table.
--
$(declareBuiltins ''Expr 'fromString
   [ "Sequence"
   , "List"
   , "Apply"
   , "Map"
   , "Plus"
   , "Subtract"
   , "Times"
   , "Divide"
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
   , "Set"
   , "SetDelayed"
   , "UpSet"
   , "UpSetDelayed"
   , "TagSetDelayed"
   , "CompoundExpression"
   , "Null"
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

instance FromExpr Bool where
  fromExpr = boolView

instance ToExpr Bool where
  toExpr = fromBool

instance FromExpr () where
  fromExpr = \case
    Null -> Just ()
    _    -> Nothing

instance ToExpr () where
  toExpr () = Null

instance FromExpr a => FromExpr [a] where
  fromExpr = \case
    ExprApp List xs -> mapM fromExpr $ Foldable.toList xs
    _               -> Nothing
instance ToExpr a => ToExpr [a] where
  toExpr xs = ExprApp List (Seq.fromList (map toExpr xs))
