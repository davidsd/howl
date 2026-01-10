{-# LANGUAGE PatternSynonyms #-}

module MicroMath.Util
  ( splits
  , splits1
  , subSequences
  ) where

import Data.Sequence (Seq, pattern (:<|))
import Data.Sequence qualified as Seq
import Data.Foldable qualified as Foldable

-- | All splits of xs into (t1,t2) where xs == t1++t2
-- 
-- Examples (writing sequences as lists):
-- > splits [2,3] = ([],[2,3]), ([2],[3]), ([2,3],[])
-- > splits [1,2,3] = ([],[1,2,3]), ([1],[2,3]), ([1,2],[3]), ([1,2,3],[])
splits :: Seq a -> [(Seq a, Seq a)]
splits xs = zip (Foldable.toList (Seq.inits xs)) (Foldable.toList (Seq.tails xs))

-- | All splits of xs into (t1, t, t2) where xs == t1++[t]++t2.
--
-- Examples:
-- > splits1 [1,2] = ([],1,[2]), ([1],2,[])
splits1 :: Seq a -> [(Seq a, a, Seq a)]
splits1 xs = do
  (l, x :<| r) <- splits xs
  pure (l, x, r)

-- | Split xs into all pairs (subSeq,rest) where subSeq is a
-- subsequence of xs and rest are the remaining elements not in
-- subSeq, with order preserved.
--
-- Examples:
-- > subSequences [1,2] = [([], [1,2]), ([1],[2]), ([2],[1]), ([1,2],[])]
--
-- Note that the number of return values is 2^(length xs). This could
-- introduce performance issues, so should be used with care.
subSequences :: Seq a -> [(Seq a, Seq a)]
subSequences =
  map (\(xs,ys) -> (Seq.fromList xs, Seq.fromList ys)) . subSequencesList . Foldable.toList

subSequencesList :: [a] -> [([a], [a])]
subSequencesList [] = [([], [])]
subSequencesList (x:xs) = do
  (s,rest) <- subSequencesList xs
  [(x:s, rest), (s, x:rest)]
