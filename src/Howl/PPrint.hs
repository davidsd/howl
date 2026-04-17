-- | Minimal pretty-printing support used throughout Howl.
module Howl.PPrint
  ( PPrint(..)
  ) where

-- | A class for converting values to a human-readable 'String'.
class PPrint a where
  pPrint :: a -> String
