{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import Data.Matrix qualified as Matrix
import Data.Matrix (Matrix)
import Data.Containers.ListUtils (nubOrd)
import Data.FileEmbed            (makeRelativeToProject, strToExp)
import Data.Foldable             qualified as Foldable
import Data.Sequence             (Seq, pattern (:<|), pattern Empty)
import Data.Sequence             qualified as Seq
import Data.Set                  (Set)
import Data.Set                  qualified as Set
import ExprTypes                 (FreeVect (..), Monomial (..), freeVectTerms, vec,
                                  mkMonic, zeroFreeVect)
import Howl

data Letter = P | Pb | Z | Zb | Psi | Psib
  deriving (Eq, Ord, Enum, Bounded)

instance Show Letter where
  show l = pPrint (letterToSymbol l)

letters :: [Letter]
letters = [minBound .. maxBound]

letterSymbolMap :: [(Letter, Symbol)]
letterSymbolMap =
  [ (P,    "P")
  , (Pb,   "Pb")
  , (Z,    "Z")
  , (Zb,   "Zb")
  , (Psi,  "ψ")
  , (Psib, "ψb")
  ]

letterToSymbol :: Letter -> Symbol
letterToSymbol l = maybe undefined id $ lookup l letterSymbolMap

symbolToLetter :: Symbol -> Maybe Letter
symbolToLetter s = lookup s [(y,x) | (x,y) <- letterSymbolMap]

instance FromExpr Letter where
  fromExpr = \case
    ExprSymbol s -> symbolToLetter s
    _            -> Nothing

instance ToExpr Letter where
  toExpr = ExprSymbol . letterToSymbol

letterCharge :: Letter -> Rational
letterCharge = \case
  P    -> 1
  Pb   -> -1
  Z    -> 1
  Zb   -> -1
  Psi  -> 1/2
  Psib -> -1/2

letterLevel :: Letter -> Rational
letterLevel = \case
  P    -> 2
  Pb   -> 2
  Z    -> 1
  Zb   -> 1
  Psi  -> 3/2
  Psib -> 3/2

newtype Tr = Tr (Seq Letter)
  deriving (Eq, Ord)

instance Show Tr where
  show (Tr xs) = "tr" <> show (Foldable.toList xs)

instance FromExpr Tr where
  fromExpr = \case
    "tr" :@ xs -> fmap Tr $ mapM fromExpr xs
    _          -> Nothing
instance ToExpr Tr where
  toExpr (Tr xs) = "tr" :@ fmap toExpr xs

-- TODO: Memoize
opsAtLevelCharge :: Rational -> Rational -> [Tr]
opsAtLevelCharge lvl chg = map Tr (go lvl chg)
  where
    go :: Rational -> Rational -> [Seq Letter]
    go level charge
      | level == 0 && charge == 0 = [Empty]
      | abs charge > level || level < 0 = []
      | otherwise = concat $ do
          l <- letters
          pure $ map (l :<|) $ go (level - letterLevel l) (charge - letterCharge l)

opsUptoLevelCharge :: Rational -> Rational -> [Tr]
opsUptoLevelCharge level charge = concat
  [ opsAtLevelCharge l charge | l <- [level, level-1 .. 0] ]

callSeq :: (ToExpr a, FromExpr b) => Expr -> Seq a -> Eval b
callSeq f xs = do
  result <- eval $ f :@ (fmap toExpr xs)
  case fromExpr result of
    Just r -> pure r
    Nothing -> error $ "Could not parse result of call: " ++ show (pPrint f, fmap (pPrint . toExpr) xs, pPrint result)

call :: (ToExpr a, FromExpr b) => Expr -> a -> Eval b
call f x = callSeq f (Seq.singleton x)

susyO2WL :: FilePath
susyO2WL = $(makeRelativeToProject "examples/susyO2/susyO2.wl" >>= strToExp)

canonicalizeZ2xZ2 :: Tr -> Eval (FreeVect Tr Rational)
canonicalizeZ2xZ2 = call "canonicalizeZ2xZ2"

-- | Take the anticommutator with Q
acommQ :: Tr -> Eval (FreeVect (Monomial Tr Int) Rational)
acommQ = call "canonZ2xZ2acommQ"

-- | Apply cyclicity of the trace
cycAny :: Tr -> Eval (FreeVect (Monomial Tr Int) Rational)
cycAny = call "canonZ2xZ2cycAny"

-- | Multiply (commutator?) by the gauge generator
gauge :: Tr -> Eval (FreeVect (Monomial Tr Int) Rational)
gauge = call "canonZ2xZ2gauge"

projectReflectionEven :: Tr -> Eval (FreeVect Tr Rational)
projectReflectionEven = call "projectReflectionEven"

projectReflectionOdd  :: Tr -> Eval (FreeVect Tr Rational)
projectReflectionOdd = call "projectReflectionOdd"

inner2 :: FreeVect Tr Rational -> FreeVect Tr Rational -> Eval (FreeVect (Monomial Tr Int) Rational)
inner2 x y = callSeq "canonZ2xZ2inner2" (Seq.fromList [x,y])

-- | Operators with the given charge and up to the given level that
-- are unique up to the action of Z2xZ2.
uniqueOpsZ2xZ2 :: Rational -> Rational -> Eval (Set Tr)
uniqueOpsZ2xZ2 level charge = do
  let ops = opsUptoLevelCharge level charge
  canonicalized <- mapM canonicalizeZ2xZ2 ops
  pure $ foldMap freeVectTerms canonicalized

-- | Canonicalize a set of equations by making the leading term have
-- coefficient 1, removing duplicates, and removing the trivial
-- equation.
collectFreeVects :: (Ord a, Ord c, Fractional c) => [FreeVect a c] -> Set (FreeVect a c)
collectFreeVects =
  Set.filter (/= zeroFreeVect) .
  Set.fromList .
  map mkMonic

kineticEqs :: Rational -> Eval (Set (FreeVect (Monomial Tr Int) Rational))
kineticEqs level = do
  ops <- fmap Set.toList $ uniqueOpsZ2xZ2 level 0
  collectFreeVects <$> mapM cycAny ops

gaugeEqs :: Rational -> Eval (Set (FreeVect (Monomial Tr Int) Rational))
gaugeEqs level = do
  ops <- fmap Set.toList $ uniqueOpsZ2xZ2 (level - 3) 0
  collectFreeVects <$> mapM gauge ops

canonicalizeReverse :: Tr -> Tr
canonicalizeReverse t@(Tr xs) = if t <= tRev then t else tRev
  where
    tRev = Tr (Seq.reverse xs)

susyEqs :: Rational -> Eval (Set (FreeVect (Monomial Tr Int) Rational))
susyEqs level = do
  let
    ops =
      nubOrd $
      map canonicalizeReverse $
      opsUptoLevelCharge (level - 1/2) (-1/2)
  collectFreeVects <$> mapM acommQ ops

data O2Rep = ZeroPlus | ZeroMinus | Charge Rational
  deriving (Eq, Ord, Show)

irrepsUpToLevel :: Rational -> O2Rep -> Eval (Set (FreeVect Tr Rational))
irrepsUpToLevel level rep = case rep of
  ZeroPlus -> do
    let chargeNeutral = opsUptoLevelCharge level 0
    collectFreeVects <$> mapM projectReflectionEven chargeNeutral
  ZeroMinus -> do
    let chargeNeutral = opsUptoLevelCharge level 0
    collectFreeVects <$> mapM projectReflectionOdd chargeNeutral
  Charge charge ->
    pure $ Set.fromList $ map vec $ opsUptoLevelCharge level charge

innerMatrix :: Rational -> O2Rep -> Eval (Matrix (FreeVect (Monomial Tr Int) Rational))
innerMatrix level rep = do
  ops <- Set.toList <$> irrepsUpToLevel level rep
  Matrix.fromLists <$>
    sequence [ sequence [ inner2 o1 o2 | o1 <- ops] | o2 <- ops ]
    
{-
irrepsUpToLevel :: Rational -> Eval (Map O2Rep (Set (FreeVect Tr Rational)))
irrepsUpToLevel level = do
  let
    higherCharge =
      [ (Charge charge, Set.fromList $ map vec $ opsUptoLevelCharge level charge)
      | charge <- [1/2, 1 .. level]
      ]
    chargeNeutral = opsUptoLevelCharge level 0
  zeroPlus  <- collectFreeVects <$> mapM projectReflectionEven chargeNeutral
  zeroMinus <- collectFreeVects <$> mapM projectReflectionOdd  chargeNeutral
  pure $ Map.fromList $
    [ (ZeroPlus, zeroPlus)
    , (ZeroMinus, zeroMinus)
    ] ++ higherCharge
-}

--myProgram :: Eval [Maybe (FreeVect (Monomial Tr) Numeric)]
myProgram :: Eval
  ( Set (FreeVect (Monomial Tr Int) Rational)
  , Set (FreeVect (Monomial Tr Int) Rational)
  , Set (FreeVect (Monomial Tr Int) Rational)
  , Matrix (FreeVect (Monomial Tr Int) Rational)
  )
myProgram = do
  addBuiltins
  get_ susyO2WL
  xs <- kineticEqs 5
  ys <- gaugeEqs 5
  zs <- susyEqs 5
  ws <- innerMatrix 6 ZeroPlus
  pure (xs, ys, zs, ws)
{-
  run_ "acommQExpand[x_]:=Expand[acommQ[x]]"
  mapM (call "acommQExpand") neutralOps
  -}

-- | Should print True
main :: IO ()
main = runEval myProgram >>= print

--runEval myProgram >>= putStrLn . pPrint
