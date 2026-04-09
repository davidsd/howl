module Howl.PPrint
  ( PPrint(..)
  ) where

class PPrint a where
  pPrint :: a -> String
