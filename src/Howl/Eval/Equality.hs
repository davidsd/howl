{-# LANGUAGE PatternSynonyms #-}

module Howl.Eval.Equality
  ( exprEqualFast
  ) where

import Data.Sequence           (Seq, pattern (:<|), pattern Empty)
import Data.Sequence qualified as Seq
import GHC.StableName          (makeStableName)
import Howl.Expr.Internal (Expr (..))
import System.IO.Unsafe        (unsafePerformIO)

-- | Fast equality check for evaluator rewrite detection.
--   Uses StableName pointer equality at the root and shallowly on
--   immediate children, falling back to structural equality per-node
--   when pointers differ.
{-# INLINE exprEqualFast #-}
exprEqualFast :: Expr -> Expr -> Bool
exprEqualFast a b = unsafePerformIO $ go a b
  where
    go :: Expr -> Expr -> IO Bool
    go (ExprLit x) (ExprLit y) = pure (x == y)
    go (ExprSymbol x) (ExprSymbol y) = pure (x == y)
    go e1@(ExprApp h1 cs1) e2@(ExprApp h2 cs2) = do
      sn1 <- makeStableName e1
      sn2 <- makeStableName e2
      if sn1 == sn2
        then pure True
        else
          if Seq.length cs1 /= Seq.length cs2
            then pure False
            else do
              headEq <- eqNode h1 h2
              if headEq then goSeq cs1 cs2 else pure False
    go _ _ = pure False

    eqNode :: Expr -> Expr -> IO Bool
    eqNode (ExprLit x) (ExprLit y) = pure (x == y)
    eqNode (ExprSymbol x) (ExprSymbol y) = pure (x == y)
    eqNode x y = do
      sx <- makeStableName x
      sy <- makeStableName y
      if sx == sy then pure True else pure (x == y)

    goSeq :: Seq Expr -> Seq Expr -> IO Bool
    goSeq Empty Empty = pure True
    goSeq (x :<| xs) (y :<| ys) = do
      xyEq <- eqNode x y
      if xyEq then goSeq xs ys else pure False
    goSeq _ _ = pure False
