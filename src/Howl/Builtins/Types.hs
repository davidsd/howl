{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}

module Howl.Builtins.Types
  ( ListOrSolo (..)
  , AList (..)
  ) where

import Data.Sequence (Seq)
import Howl.Expr     (FromExpr (..), ToExpr (..), pattern (:@), pattern List)

-- | A datatype that matches a single expression e or a list of
-- expressions {e1,...,en}, such that the expressions can all be
-- mapped to the type 'a'.
newtype ListOrSolo a = MkListOrSolo (Seq a)

instance FromExpr a => FromExpr (ListOrSolo a) where
  fromExpr = fmap MkListOrSolo . \case
    List :@ es -> mapM fromExpr es
    e          -> fmap pure $ fromExpr e

newtype AList a = MkList { unList :: Seq a }

instance FromExpr a => FromExpr (AList a) where
  fromExpr = \case
    List :@ xs -> fmap MkList (mapM fromExpr xs)
    _          -> Nothing

instance ToExpr a => ToExpr (AList a) where
  toExpr (MkList xs) = List :@ fmap toExpr xs
