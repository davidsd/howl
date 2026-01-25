module MicroMath.PPrint
  ( PPrint(..)
  ) where

class PPrint a where
  pPrint :: a -> String
