{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

-- | Logical and comparison builtins.
module Howl.Builtins.Logic
  ( addLogicBuiltins
  ) where

import Data.Sequence           (Seq, pattern (:<|), pattern Empty)
import Howl.Builtins.ToBuiltin (Variadic (..), def)
import Howl.Eval.Context       (Eval, modifyAttributes, setFlat)
import Howl.Expr               (Expr (..), pattern (:@), pattern And,
                                pattern ExprNumeric, pattern Or)
import Howl.Expr               qualified as Expr
import Howl.Util               (pattern Solo)

---------- OrderedQ ----------

-- | OrderedQ[h[x1,...,xn]] tests whether the xi's are in the
-- canonical order defined by the Haskell ordering on expressions
-- TODO: Implement a general predicate
orderedQDef :: Expr -> Maybe Bool
orderedQDef = \case
  _ :@ Empty      -> Just True
  _ :@ (x :<| xs) -> Just $ go x xs
  _               -> Nothing
  where
    go _ Empty = True
    go prev (y :<| ys)
      | prev <= y = go y ys
      | otherwise = False

---------- SameQ ----------

sameQDef :: Seq Expr -> Bool
sameQDef = \case
  Empty      -> True
  x :<| rest -> all (== x) rest

---------- And ----------

normalizeAnd :: Seq Expr -> Expr
normalizeAnd args = case filterBools args of
  Nothing       -> Expr.False
  Just Empty    -> Expr.True
  Just (Solo x) -> x
  Just xs       -> And :@ xs
  where
    filterBools Empty                = Just Empty
    filterBools (Expr.False :<| _)   = Nothing
    filterBools (Expr.True :<| rest) = filterBools rest
    filterBools (x :<| xs)           = fmap (x :<|) (filterBools xs)

---------- Or ----------

normalizeOr :: Seq Expr -> Expr
normalizeOr args = case filterBools args of
  Nothing       -> Expr.True
  Just Empty    -> Expr.False
  Just (Solo x) -> x
  Just xs       -> Or :@ xs
  where
    filterBools Empty                 = Just Empty
    filterBools (Expr.True :<| _)     = Nothing
    filterBools (Expr.False :<| rest) = filterBools rest
    filterBools (x :<| xs)            = fmap (x :<|) (filterBools xs)

---------- Comparisons ----------

lessDef :: Expr -> Expr -> Maybe Bool
lessDef (ExprNumeric a) (ExprNumeric b) = Just $ a < b
lessDef x y | x == y                    = Just False
lessDef _ _                             = Nothing

lessEqualDef :: Expr -> Expr -> Maybe Bool
lessEqualDef (ExprNumeric a) (ExprNumeric b) = Just $ a <= b
lessEqualDef x y | x == y                    = Just True
lessEqualDef _ _                             = Nothing

greaterDef :: Expr -> Expr -> Maybe Bool
greaterDef (ExprNumeric a) (ExprNumeric b) = Just $ a > b
greaterDef x y | x == y                    = Just False
greaterDef _ _                             = Nothing

greaterEqualDef :: Expr -> Expr -> Maybe Bool
greaterEqualDef (ExprNumeric a) (ExprNumeric b) = Just $ a >= b
greaterEqualDef x y | x == y                    = Just True
greaterEqualDef _ _                             = Nothing

equalDef :: Expr -> Expr -> Maybe Bool
equalDef (ExprNumeric a) (ExprNumeric b) = Just $ a == b
equalDef x y | x == y                    = Just True
equalDef _ _                             = Nothing

-- | Register the logical and comparison builtins.
--
-- This defines:
--
-- - @And@
-- - @Or@
-- - @Identity@
-- - @Equal@
-- - @Greater@
-- - @Less@
-- - @GreaterEqual@
-- - @LessEqual@
-- - @SameQ@
-- - @OrderedQ@
-- - @EvenQ@
-- - @OddQ@
addLogicBuiltins :: Eval ()
addLogicBuiltins = do
  modifyAttributes "And" setFlat
  def "And" (MkVariadic normalizeAnd)

  modifyAttributes "Or" setFlat
  def "Or" (MkVariadic normalizeOr)

  def "Identity" (id @Expr)
  def "Equal" equalDef
  def "Greater" greaterDef
  def "Less" lessDef
  def "GreaterEqual" greaterEqualDef
  def "LessEqual" lessEqualDef
  def "SameQ" (MkVariadic sameQDef)
  def "OrderedQ" orderedQDef
  def "EvenQ" (even @Integer)
  def "OddQ" (odd @Integer)
