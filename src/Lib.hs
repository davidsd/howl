module Lib where

import Matching2 ()

someFunc :: IO ()
someFunc = putStrLn "Hello world!"

type Name = String

newtype Var = MkVar Name
  deriving (Eq, Ord, Show)

newtype SeqVar = MkSeqVar Name
  deriving (Eq, Ord, Show)

newtype FunVar = MkFunVar Name
  deriving (Eq, Ord, Show)

newtype Symbol = MkSymbol Name
  deriving (Eq, Ord, Show)

data FunSymbol
  = FreeFun Name
  | CFun Name
  | AFun Name
  | ACFun Name
  deriving (Eq, Ord, Show)

{-
-}

-- | TODO: Can we add a TermSymbol and get rid of TermVarApp?
data Term
  = TermVar Var                -- x
  | TermApp FunSymbol [SeqElt] -- f(s1, ..., sn)
  | TermVarApp FunVar [SeqElt] -- X(s1, ..., sn)
  deriving (Eq, Ord, Show)

data SeqElt
  = SeqEltTerm Term     -- t
  | SeqEltSeqVar SeqVar -- \bar{x}
  deriving (Eq, Ord, Show)
  
data SolvedEq 
  = SolvedVar Var Term                           -- x ~ t
  | SolvedFunVar FunVar FunSymbol                -- X ~ g
  | SolvedSeqVar SeqVar [Term]                   -- \bar{x} ~ \tl{r}
  | SolvedSeqVarMulti SeqVar [Term]              -- \bar{x} ~ {{ \tl r }}
  | SolvedSeqVarApp SeqVar [Term] FunSymbol      -- \bar{x} ~ \tl t[f]
  | SolvedSeqVarAppMulti SeqVar [Term] FunSymbol -- \bar{x} ~ {{ \tl t }}[f]
  deriving (Eq, Ord, Show)

data MatchingEq = MkMatchingEq Term Term
  deriving (Eq, Ord, Show)

-- split [2,3] = ([],[2,3]), ([2],[3]), ([2,3],[])
-- split [1,2,3] = ([],[1,2,3]), ([1],[2,3]), ([1,2],[3]), ([1,2,3],[])
split :: [a] -> [([a],[a])]
split [] = [([], [])]
split (x:xs) = ([], x:xs) : do
  (xs', ys') <- split xs
  pure (x : xs', ys')

split1 :: [a] -> [([a], a, [a])]
split1 xs = do
  (l, x:r) <- split xs
  pure (l,x,r)

-- subSequences [1,2] = [([], [1,2]), ([1],[2]), ([2],[1]), ([1,2],[])]
subSequences :: [a] -> [([a], [a])]
subSequences [] = [([], [])]
subSequences (x:xs) = do
  (s,rest) <- subSequences xs
  [(x:s, rest), (s, x:rest)]

allTerms :: [SeqElt] -> Maybe [Term]
allTerms [] = Just []
allTerms (SeqEltTerm t : ss) = fmap (t:) (allTerms ss)
allTerms _ = Nothing

-- t,r: Terms
-- s,q: Sequence elements
-- \tl{t}, \tl{r}: Sequences of terms
-- \tl{s}, \tl{q}: Sequences of sequence elements

match :: MatchingEq -> [([SolvedEq], [MatchingEq])]

-- | T: Trivial
match (MkMatchingEq s s') | s == s' = [([], [])]

-- | IVE: Individual variable elimination
match (MkMatchingEq (TermVar x) t) = [([SolvedVar x t], [])]

-- | FVE: Function variable elimination
match (MkMatchingEq (TermVarApp xFun sSeq) rhs@(TermApp f tSeq))
  | Just _ <- allTerms tSeq
  = [([SolvedFunVar xFun f], [MkMatchingEq (TermApp f sSeq) rhs])]

-- | Dec-F: Decomposition under free head
match (MkMatchingEq
        (TermApp f@(FreeFun _) (SeqEltTerm s : sTilde))
        (TermApp g             (SeqEltTerm t : tTilde)))
  | f == g
  = [([], [MkMatchingEq s t, MkMatchingEq (TermApp f sTilde) (TermApp f tTilde)])]

-- | SVE-F: Sequence variable elimination under free head
match (MkMatchingEq
        (TermApp f@(FreeFun _) (SeqEltSeqVar x : sTilde))
        (TermApp g             tTildes))
  | f == g
  , Just terms <- allTerms tTildes
  = do
      (tTilde1, tTilde2) <- split terms
      pure ([SolvedSeqVar x tTilde1], [MkMatchingEq (TermApp f sTilde) (TermApp f (map SeqEltTerm tTilde2))])

-- | Dec-C: Decomposition under commutative head
match (MkMatchingEq
        (TermApp f@(CFun _) (SeqEltTerm s : sTilde))
        (TermApp g          args))
  | f == g
  , Just terms <- allTerms args
  = do
      (tTilde1, t, tTilde2) <- split1 terms
      pure ([], [ MkMatchingEq s t
                , MkMatchingEq
                  (TermApp f sTilde)
                  (TermApp f (map SeqEltTerm (tTilde1 ++ tTilde2)))
                ])
  
-- | SVE-C: Sequence variable elimination under commutative head
match (MkMatchingEq
       (TermApp f@(CFun _) (SeqEltSeqVar x : sTilde))
       (TermApp g          args))
  | f == g
  , Just terms <- allTerms args
  = do
      (subSeq, rest) <- subSequences terms
      pure ( [SolvedSeqVarMulti x subSeq]
           , [ MkMatchingEq
               (TermApp f sTilde)
               (TermApp f (map SeqEltTerm rest))
             ]
           )
        
-- | Associative head
--
-- Note that Dec-A overlaps with FVE-A and IVE-A. It says in the paper
-- that the form of the rules guarantees that no two rules aply to the
-- same equation. What gives?
--
-- Actually on page 42, it does say there can be multiple rules that
-- apply to a given equation, specifically it mentions Dec-A-strict
-- and IVE-A-strict. So it seems we have to check all three rules and
-- concatenate the results.
match (MkMatchingEq
       (TermApp f@(AFun _) (SeqEltTerm s : sTilde))
       (TermApp g          tTilde))
  | f == g
  = concat
    [
      -- | Dec-A: Decomposition under associative head
      -- NB: This is the same as Dec-F
      case tTilde of
        SeqEltTerm t : tTilde' ->
          [([], [MkMatchingEq s t, MkMatchingEq (TermApp f sTilde) (TermApp f tTilde')])]
        _ -> []

    , case s of
        -- | FVE-A: Function variable elimination under associative head
        TermVarApp xFun sTilde1
          | Just _ <- allTerms tTilde ->
            [ ( [SolvedFunVar xFun f]
              , [MkMatchingEq
                 (TermApp f (sTilde1 ++ sTilde))
                 (TermApp f tTilde)
                ]
              )
            ]
        -- | IVE-A: Individual variable elimination under associative head
        TermVar x
          | Just terms <- allTerms tTilde -> do
              (tTilde1, tTilde2) <- split terms
              pure ( [SolvedVar x (TermApp f (map SeqEltTerm tTilde1))]
                   , [MkMatchingEq
                      (TermApp f sTilde)
                      (TermApp f (map SeqEltTerm tTilde2))
                     ]
                   )
        _ -> []
    ]

-- | SVE-A: Sequence variable elimination under associative head
match (MkMatchingEq
       (TermApp f@(AFun _) (SeqEltSeqVar x : sTilde))
       (TermApp g          args))
  | f == g
  , Just terms <- allTerms args
  = do
      (tTilde1, tTilde2) <- split terms
      pure ( [SolvedSeqVarApp x tTilde1 f]
           , [MkMatchingEq
              (TermApp f sTilde)
              (TermApp f (map SeqEltTerm tTilde2))
             ]
           )

match (MkMatchingEq
        (TermApp f@(ACFun _) (SeqEltTerm s : sTilde))
        (TermApp g args))
  | f == g
  , Just terms <- allTerms args
  = concat
    [
      -- | Dec-AC: Decomposition under AC head
      do
        (tTilde1, t, tTilde2) <- split1 terms
        pure ([],
               [ MkMatchingEq s t
               , MkMatchingEq
                 (TermApp f sTilde)
                 (TermApp f (map SeqEltTerm (tTilde1 <> tTilde2)))
               ]
             )
    , case s of
        -- | FVE-AC: Function variable elimination under AC head
        TermVarApp xFun sTilde1 ->
          [ ( [SolvedFunVar xFun f]
            , [MkMatchingEq
                (TermApp f (sTilde1 <> sTilde))
                (TermApp f (map SeqEltTerm terms))
              ]
            )
          ]
        -- | IVE-AC: Individual variable elimination under AC head
        TermVar x -> do
          (subSeq, rest) <- subSequences terms
          pure ( [SolvedVar x (TermApp f (map SeqEltTerm subSeq))]
               , [MkMatchingEq
                  (TermApp f sTilde)
                  (TermApp f (map SeqEltTerm rest))
                 ]
               )
        _ -> []
    ]

-- | SVE-AC: Sequence variable elimination under AC head
match (MkMatchingEq
       (TermApp f@(ACFun _) (SeqEltSeqVar x : sTilde))
       (TermApp g args))
  | f == g
  , Just terms <- allTerms args
  = do
      (subSeq, rest) <- subSequences terms
      pure ( [SolvedSeqVarAppMulti x subSeq f]
           , [MkMatchingEq
              (TermApp f sTilde)
              (TermApp f (map SeqEltTerm rest))
             ]
           )
