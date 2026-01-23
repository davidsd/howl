{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}

module MicroMath.Util
  ( splits
  , splits1
  , subSequences
  , pattern Solo
  , pattern Pair
  , mapMaybeSeq
  ) where

import Data.Foldable qualified as Foldable
import Data.Sequence (Seq, pattern (:<|), pattern Empty)
import Data.Sequence qualified as Seq

-- | All splits of xs into (t1,t2) where xs == t1++t2
--
-- Examples (writing sequences as lists):
-- > splits [2,3] = ([],[2,3]), ([2],[3]), ([2,3],[])
-- > splits [1,2,3] = ([],[1,2,3]), ([1],[2,3]), ([1,2],[3]), ([1,2,3],[])
{-# INLINE splits #-}
splits :: Seq a -> [(Seq a, Seq a)]
splits xs = zip (Foldable.toList (Seq.inits xs)) (Foldable.toList (Seq.tails xs))

-- | All splits of xs into (t1, t, t2) where xs == t1++[t]++t2.
--
-- Examples:
-- > splits1 [1,2] = ([],1,[2]), ([1],2,[])
{-# INLINE splits1 #-}
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
{-# INLINE subSequences #-}
subSequences :: Seq a -> [(Seq a, Seq a)]
subSequences = \case
  Empty -> [(Empty, Empty)]
  x :<| xs' ->
    let
      rec = subSequences xs'
    in
      [ (x :<| s, rest) | (s, rest) <- rec ] ++
      [ (s, x :<| rest) | (s, rest) <- rec ]

{-# INLINE Solo #-}
pattern Solo :: a -> Seq a
pattern Solo x = x :<| Empty

{-# INLINE Pair #-}
pattern Pair :: a -> a -> Seq a
pattern Pair x y = x :<| y :<| Empty

{-# INLINE mapMaybeSeq #-}
mapMaybeSeq :: (a -> Maybe b) -> Seq a -> Seq b
mapMaybeSeq _ Empty = Empty
mapMaybeSeq f (x :<| xs)
  | Just y <- f x = y :<| mapMaybeSeq f xs
  | otherwise     = mapMaybeSeq f xs
