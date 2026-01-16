{-# OPTIONS_GHC -fno-warn-orphans #-}

module MicroMath.Symbol
  ( Symbol
  , symbolIndex
  , symbolToShortText
  , symbolFromShortText
  ) where

import Data.Text.Short (ShortText)
import Data.Hashable    (hash)
import MicroMath.PPrint (PPrint (..))
import Symbolize        (Symbol)
import Symbolize        qualified as Symbolize

instance PPrint Symbol where
  pPrint = Symbolize.unintern

-- | The index to use for a symbol in an IntMap. Note that Symbolize
-- guarantees that hashing is a no-op and hash will never collide.
symbolIndex :: Symbol -> Int
symbolIndex = hash

symbolToShortText :: Symbol -> ShortText
symbolToShortText = Symbolize.unintern

symbolFromShortText :: ShortText -> Symbol
symbolFromShortText = Symbolize.intern
