{-# OPTIONS_GHC -fno-warn-orphans #-}

module MicroMath.Symbol
  ( Symbol
  , symbolIndex
  ) where

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
