{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}

module MicroMath.Util
  ( splitsK
  , splits1K
  , subSequencesK
  , pattern Solo
  , pattern Pair
  , mapMaybeSeq
  ) where

import Data.Sequence (Seq, ViewL (..), (|>), pattern (:<|), pattern Empty)
import Data.Sequence qualified as Seq

-- | Stream splits of xs into (t1,t2) where xs == t1++t2.
--   Calls the step function once per split; allows early exit.
{-# INLINE splitsK #-}
splitsK :: Seq a -> ((Seq a, Seq a) -> r -> r) -> r -> r
splitsK xs step z = go Seq.empty xs
  where
    go left right =
      let rest = case Seq.viewl right of
            EmptyL   -> z
            x :< xs' -> go (left |> x) xs'
      in step (left, right) rest

-- | Stream splits of xs into (t1, t, t2) where xs == t1++[t]++t2.
--   Calls the step function once per split; allows early exit.
{-# INLINE splits1K #-}
splits1K :: Seq a -> ((Seq a, a, Seq a) -> r -> r) -> r -> r
splits1K xs step z = go Seq.empty xs
  where
    go left right =
      case Seq.viewl right of
        EmptyL   -> z
        x :< xs' ->
          let rest = go (left |> x) xs'
          in step (left, x, xs') rest

-- | Stream all (subSeq,rest) pairs where subSeq is a subsequence of xs.
--   Calls the step function once per pair; allows early exit.
{-# INLINE subSequencesK #-}
subSequencesK :: Seq a -> ((Seq a, Seq a) -> r -> r) -> r -> r
subSequencesK xs step z = case Seq.viewl xs of
  EmptyL   -> step (Seq.empty, Seq.empty) z
  x :< xs' ->
    subSequencesK xs' (\(s, rest) r ->
      step (x :<| s, rest) (step (s, x :<| rest) r)
    ) z

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
