{-# OPTIONS_GHC -fno-warn-orphans #-}

module MicroMath.Symbol.Symbolize
  ( Symbol
  , symbolToShortText
  , symbolFromShortText
  ) where

import Data.Text.Short  (ShortText)
import MicroMath.PPrint (PPrint (..))
import Symbolize        (Symbol)
import Symbolize        qualified as Symbolize

instance PPrint Symbol where
  pPrint = Symbolize.unintern

symbolToShortText :: Symbol -> ShortText
symbolToShortText = Symbolize.unintern

symbolFromShortText :: ShortText -> Symbol
symbolFromShortText = Symbolize.intern
