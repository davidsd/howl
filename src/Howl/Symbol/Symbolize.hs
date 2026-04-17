{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Symbol implementation backed by the @symbolize@ package.
module Howl.Symbol.Symbolize
  ( Symbol
  , symbolToShortText
  , symbolFromShortText
  ) where

-- | An interned symbol name.
import Data.Text.Short (ShortText)
import Howl.PPrint     (PPrint (..))
import Symbolize       (Symbol)
import Symbolize       qualified as Symbolize

instance PPrint Symbol where
  pPrint = Symbolize.unintern

-- | Convert a symbol to its short-text name.
symbolToShortText :: Symbol -> ShortText
symbolToShortText = Symbolize.unintern

-- | Intern a short-text name as a symbol.
symbolFromShortText :: ShortText -> Symbol
symbolFromShortText = Symbolize.intern
