{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Howl.Util
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
splitsK :: Seq a -> (Seq a -> Seq a -> r -> r) -> r -> r
splitsK xs step z = go Seq.empty xs
  where
    go left right =
      let rest = case Seq.viewl right of
            EmptyL   -> z
            x :< xs' -> go (left |> x) xs'
      in step left right rest

-- | Stream splits of xs into (t1, t, t2) where xs == t1++[t]++t2.
--   Calls the step function once per split; allows early exit.
{-# INLINE splits1K #-}
splits1K :: Seq a -> (Seq a -> a -> Seq a -> r -> r) -> r -> r
splits1K xs step z = go Seq.empty xs
  where
    go left right =
      case Seq.viewl right of
        EmptyL   -> z
        x :< xs' ->
          let rest = go (left |> x) xs'
          in step left x xs' rest

-- | Enumerate complete subsequences for sets up to this size
maxCompleteSubsequences :: Int
maxCompleteSubsequences = 10

-- | For sets of size bigger than maxCompleteSubsequences, enumerate
-- subsequences up to this size.
maxSubseqSize :: Int
maxSubseqSize = 3

-- | Stream all (subSeq,rest) pairs where subSeq is a subsequence of xs.
--   Enumerates in increasing order of subsequence length. For long
--   sequences, only subsequences up to a small size are generated.
{-# INLINE subSequencesK #-}
subSequencesK :: forall a r. Seq a -> (Seq a -> Seq a -> r -> r) -> r -> r
subSequencesK xs step z =
  let
    n = Seq.length xs
    maxLen = if n <= maxCompleteSubsequences then n else maxSubseqSize
    go :: Seq a -> Int -> Seq a -> Seq a -> r -> r
    go ys len chosen rest z0 =
      case Seq.viewl ys of
        EmptyL ->
          if len == 0
            then step chosen rest z0
            else z0
        x :< xs' ->
          let z1 = go xs' len chosen (rest |> x) z0
          in if len > 0
            then go xs' (len - 1) (chosen |> x) rest z1
            else z1
    loop len z0 = go xs len Seq.empty Seq.empty z0
  in
    foldr loop z [0..maxLen]

-- | A sequence of length 1 (bidirectional pattern synonym)
{-# INLINE Solo #-}
pattern Solo :: a -> Seq a
pattern Solo x = x :<| Empty

-- | A sequence of length 2 (bidirectional pattern synonym)
{-# INLINE Pair #-}
pattern Pair :: a -> a -> Seq a
pattern Pair x y = x :<| y :<| Empty

-- | Map a function over a 'Seq', collecting 'Just' results
{-# INLINE mapMaybeSeq #-}
mapMaybeSeq :: (a -> Maybe b) -> Seq a -> Seq b
mapMaybeSeq _ Empty = Empty
mapMaybeSeq f (x :<| xs)
  | Just y <- f x = y :<| mapMaybeSeq f xs
  | otherwise     = mapMaybeSeq f xs
