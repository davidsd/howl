{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

module Howl.Expr.Syntax
  ( pattern Sequence
  , pattern List
  , pattern Apply
  , pattern Map
  , pattern Plus
  , pattern Subtract
  , pattern Times
  , pattern Divide
  , pattern Power
  , pattern Blank
  , pattern BlankSequence
  , pattern BlankNullSequence
  , pattern Slot
  , pattern SlotSequence
  , pattern Pattern
  , pattern Alternatives
  , pattern Optional
  , pattern Test
  , pattern PatternTest
  , pattern ConfirmPatternTest
  , pattern Association
  , pattern And
  , pattern Or
  , pattern True
  , pattern False
  , pattern Less
  , pattern Greater
  , pattern LessEqual
  , pattern GreaterEqual
  , pattern Equal
  , pattern Unequal
  , pattern SameQ
  , pattern UnsameQ
  , pattern Rule
  , pattern RuleDelayed
  , pattern Set
  , pattern SetDelayed
  , pattern UpSet
  , pattern UpSetDelayed
  , pattern TagSetDelayed
  , pattern Part
  , pattern Default
  , pattern CompoundExpression
  , pattern Null
  ) where

import Data.Foldable      qualified as Foldable
import Data.Sequence      qualified as Seq
import Data.String        (fromString)
import Howl.Expr.Internal (Expr (..), FromExpr (..), ToExpr (..))
import Howl.Expr.TH       (declareBuiltins)
import Prelude            hiding (False, True)
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
   , "PatternTest"
   , "ConfirmPatternTest"
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
   , "RuleDelayed"
   , "Set"
   , "SetDelayed"
   , "UpSet"
   , "UpSetDelayed"
   , "TagSetDelayed"
   , "Part"
   , "Default"
   , "CompoundExpression"
   , "Null"
   ])

instance FromExpr Bool where
  fromExpr = \case
    True  -> Just Prelude.True
    False -> Just Prelude.False
    _     -> Nothing

instance ToExpr Bool where
  toExpr Prelude.True  = True
  toExpr Prelude.False = False

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
