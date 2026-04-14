{-# OPTIONS_GHC -fno-warn-orphans #-}

module Howl.Symbol.Simple
  ( Symbol
  , symbolToShortText
  , symbolFromShortText
  ) where

import Data.Hashable   (Hashable)
import Data.String     (IsString (..))
import Data.Text.Short (ShortText)
import Data.Text.Short qualified as ShortText
import Howl.PPrint     (PPrint (..))

newtype Symbol = MkSymbol ShortText
  deriving (Eq, Ord, Show, Hashable)

symbolToShortText :: Symbol -> ShortText
symbolToShortText (MkSymbol name) = name

symbolFromShortText :: ShortText -> Symbol
symbolFromShortText = MkSymbol

instance IsString Symbol where
  fromString = symbolFromShortText . fromString

instance PPrint Symbol where
  pPrint (MkSymbol sym) = ShortText.unpack sym
