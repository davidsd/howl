{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

-- | Bidirectional pattern synonyms for common Wolfram Language symbol
-- expressions.
--
-- This module exports names such as 'Plus', 'List', and 'Rule' as
-- bidirectional pattern synonyms. They can be used both for matching
-- expressions and for constructing them.
--
-- For example:
--
-- - @Plus :@ Seq.fromList [toExpr 1, "x"]@ represents the Wolfram
--   Language expression @1 + x@
-- - @Rule :@ Seq.fromList ["x", toExpr 3]@ represents the Wolfram
--   Language expression @x -> 3@
--
-- Compared to "Howl.Expr", this module focuses on the larger catalog
-- of named symbol-expression patterns.
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
import Data.Sequence      (pattern (:<|), pattern Empty)
import Data.Sequence      qualified as Seq
import Data.String        (fromString)
import Howl.Expr.Internal (Expr (..), FromExpr (..), ToExpr (..))
import Howl.Expr.TH       (declareExprPatterns)
import Prelude            hiding (False, True)
import Prelude qualified

-- declareExprPatterns creates bidirectional pattern synonyms Sequence,
-- List, etc. The main advantage of these over using the IsString
-- instance for Expr and writing "Sequence", "List", etc, is that for
-- the pattern synonyms we call mkSymbol at top level precisely once,
-- whereas each time our code uses the IsString instance, it needs to
-- use 'mkSymbol' which hashes the given text and looks it up in the
-- symbol table.
--
$(declareExprPatterns ''Expr 'fromString
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

instance (FromExpr a, FromExpr b) => FromExpr (a, b) where
  fromExpr = \case
    ExprApp List (a :<| b :<| Empty) -> (,) <$> fromExpr a <*> fromExpr b
    _                                -> Nothing
instance (ToExpr a, ToExpr b) => ToExpr (a, b) where
  toExpr (a, b) = ExprApp List (Seq.fromList [toExpr a, toExpr b])

instance (FromExpr a, FromExpr b, FromExpr c) => FromExpr (a, b, c) where
  fromExpr = \case
    ExprApp List (a :<| b :<| c :<| Empty) ->
      (,,) <$> fromExpr a <*> fromExpr b <*> fromExpr c
    _ -> Nothing
instance (ToExpr a, ToExpr b, ToExpr c) => ToExpr (a, b, c) where
  toExpr (a, b, c) = ExprApp List (Seq.fromList [toExpr a, toExpr b, toExpr c])

instance (FromExpr a, FromExpr b, FromExpr c, FromExpr d) => FromExpr (a, b, c, d) where
  fromExpr = \case
    ExprApp List (a :<| b :<| c :<| d :<| Empty) ->
      (,,,) <$> fromExpr a <*> fromExpr b <*> fromExpr c <*> fromExpr d
    _ -> Nothing
instance (ToExpr a, ToExpr b, ToExpr c, ToExpr d) => ToExpr (a, b, c, d) where
  toExpr (a, b, c, d) =
    ExprApp List (Seq.fromList [toExpr a, toExpr b, toExpr c, toExpr d])
