{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | The public symbol type used in Howl expressions.
--
-- Symbols are interned names such as @x@, @Plus@, or @List@.
-- By default, Howl uses the @symbolize@ library as its symbol
-- implementation. There is also a simpler implementation in
-- "Howl.Symbol.Simple" based on a @newtype@ over 'Data.Text.Short.ShortText',
-- but it is currently turned off by default.
module Howl.Symbol
  ( Symbol
  , symbolToShortText
  , symbolFromShortText
  ) where

--import Howl.Symbol.Simple
import Howl.Symbol.Symbolize
