{-# LANGUAGE PatternSynonyms #-}

{-| This module implements a version of equality that uses StableNames
  to try to short-circuit the equality check. It does not seem to
  improve performance, so it is not currently used.
-}

module MicroMath.Eval.Equality where

import Debug.Trace qualified as Debug
import Data.Sequence           (pattern (:<|), pattern Empty)
import GHC.StableName          (makeStableName)
import MicroMath.Expr.Internal (Expr (..))
import System.IO.Unsafe        (unsafePerformIO)

{-# NOINLINE checkEquality #-}
checkEquality :: Expr -> Expr -> Bool
checkEquality e1 e2 = unsafePerformIO $ checkEqualityIO e1 e2

-- | A version of (==) that recursively uses 'StableName's to check
-- equality for ExprApp's. Essentially, it checks whether the two
-- expressions have the same location in memory to decide whether they
-- are equal without traversing deeper into the tree. If two
-- 'StableName's are not equal, then the expressions might still be
-- equal, and we must check their heads and children to find out.
checkEqualityIO :: Expr -> Expr -> IO Bool
checkEqualityIO (ExprLit x)    (ExprLit y)    = pure $ x == y
checkEqualityIO (ExprSymbol x) (ExprSymbol y) = pure $ x == y
checkEqualityIO e1@(ExprApp h1 cs1) e2@(ExprApp h2 cs2) = do
  sn1 <- makeStableName e1
  sn2 <- makeStableName e2
  if sn1 == sn2
    then Debug.traceShow ("short-circuited equality" :: String) $ pure True
    else go (h1 :<| cs1) (h2 :<| cs2)
  where
    go Empty Empty = pure True
    go (x :<| xs) (y :<| ys) = do
      xyEq <- checkEqualityIO x y
      if xyEq
        then go xs ys
        else pure False
    go _ _ = pure False
checkEqualityIO _ _ = pure False
