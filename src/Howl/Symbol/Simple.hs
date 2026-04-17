{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | A simple symbol implementation backed directly by 'ShortText'.
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

-- | An interned symbol name.
newtype Symbol = MkSymbol ShortText
  deriving (Eq, Ord, Show, Hashable)

-- | Convert a symbol to its short-text name.
symbolToShortText :: Symbol -> ShortText
symbolToShortText (MkSymbol name) = name

-- | Construct a symbol from a short-text name.
symbolFromShortText :: ShortText -> Symbol
symbolFromShortText = MkSymbol

instance IsString Symbol where
  fromString = symbolFromShortText . fromString

instance PPrint Symbol where
  pPrint (MkSymbol sym) = ShortText.unpack sym
