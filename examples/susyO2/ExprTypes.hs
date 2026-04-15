{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}

module ExprTypes where

import Data.Foldable   qualified as Foldable
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence   (pattern (:<|))
import Data.Sequence   qualified as Seq
import Data.Set        (Set)
import Howl

data Power a n = MkPower a n
  deriving (Eq, Ord, Show)

instance (FromExpr a, FromExpr n, Num n) => FromExpr (Power a n) where
  fromExpr = \case
    Power :@ Pair baseExpr powExpr -> do
      x <- fromExpr baseExpr
      n <- fromExpr powExpr
      pure $ MkPower x n
    expr -> fmap (flip MkPower 1) (fromExpr expr)

newtype Monomial a n = MkMonomial (Map a n)
  deriving (Eq, Ord, Show)

instance (Ord a, FromExpr a, FromExpr n, Num n) => FromExpr (Monomial a n) where
  fromExpr = \case
    Times :@ xs -> do
      factors <- mapM fromExpr xs
      pure $ MkMonomial $ Map.fromList $ do
        MkPower x n <- Foldable.toList factors
        pure (x, n)
    expr -> do
      MkPower x n <- fromExpr expr
      pure $ MkMonomial $ Map.singleton x n

instance (ToExpr a, ToExpr n) => ToExpr (Monomial a n) where
  toExpr (MkMonomial m) =
    Times :@
    Seq.fromList
    [binary Power (toExpr base) (toExpr n) | (base, n) <- Map.toList m]

data Term c a = MkTerm c a
  deriving (Eq, Ord, Show)

instance (FromExpr a, FromNumeric c, Num c) => FromExpr (Term c a) where
  fromExpr = \case
    Times :@ (ExprNumeric n :<| xs)
      | Just c <- fromNumeric n -> fmap (MkTerm c) $ fromExpr $
        case xs of
          Solo x -> x
          _      -> Times :@ xs
      | otherwise -> error $ "Couldn't convert numeric coefficient: " ++ show n
    expr -> fmap (MkTerm 1) $ fromExpr expr

newtype FreeVect b a = MkFreeVect (Map b a)
  deriving (Eq, Ord, Show)

freeVectTerms :: FreeVect b a -> Set b
freeVectTerms (MkFreeVect m) = Map.keysSet m

mkMonic :: Fractional a => FreeVect b a -> FreeVect b a
mkMonic fv@(MkFreeVect m) = case Map.toList m of
  []                  -> fv
  (_, leadingCoeff):_ -> MkFreeVect $ fmap (/ leadingCoeff) m

zeroFreeVect :: FreeVect b a
zeroFreeVect = MkFreeVect Map.empty

vec :: Num a => b -> FreeVect b a
vec x = MkFreeVect $ Map.singleton x 1

instance (Ord a, FromExpr a, FromNumeric c, Num c) => FromExpr (FreeVect a c) where
  fromExpr = \case
    ExprInteger 0 -> pure zeroFreeVect
    Plus :@ xs -> do
      terms <- mapM fromExpr xs
      pure $ MkFreeVect $ Map.fromList $ do
        MkTerm coeff term <- Foldable.toList terms
        pure (term, coeff)
    expr -> do
      MkTerm coeff term <- fromExpr expr
      pure $ MkFreeVect $ Map.singleton term coeff

instance (ToExpr a, ToExpr c) => ToExpr (FreeVect a c) where
  toExpr (MkFreeVect m) =
    Plus :@
    Seq.fromList
    [binary Times (toExpr coeff) (toExpr term) | (term, coeff) <- Map.toList m]
