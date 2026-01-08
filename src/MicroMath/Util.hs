module MicroMath.Util
  ( splits
  , splits1
  , subSequences
  ) where

-- | All splits of xs into (t1,t2) where xs == t1++t2
-- 
-- Examples:
-- > splits [2,3] = ([],[2,3]), ([2],[3]), ([2,3],[])
-- > splits [1,2,3] = ([],[1,2,3]), ([1],[2,3]), ([1,2],[3]), ([1,2,3],[])
splits :: [a] -> [([a],[a])]
splits [] = [([], [])]
splits (x:xs) = ([], x:xs) : do
  (xs', ys') <- splits xs
  pure (x : xs', ys')

-- | All splits of xs into (t1, t, t2) where xs == t1++[t]++t2.
--
-- Examples:
-- > splits1 [1,2] = ([],1,[2]), ([1],2,[])
splits1 :: [a] -> [([a], a, [a])]
splits1 xs = do
  (l, x:r) <- splits xs
  pure (l,x,r)

-- | Split xs into all pairs (subSeq,rest) where subSeq is a
-- subsequence of xs and rest are the remaining elements not in
-- subSeq, with order preserved.
--
-- Examples:
-- > subSequences [1,2] = [([], [1,2]), ([1],[2]), ([2],[1]), ([1,2],[])]
--
-- Note that the number of return values is 2^(length xs). This could
-- introduce performance issues, so should be used with care.
subSequences :: [a] -> [([a], [a])]
subSequences [] = [([], [])]
subSequences (x:xs) = do
  (s,rest) <- subSequences xs
  [(x:s, rest), (s, x:rest)]
