module MicroMath.PPrint where

class PPrint a where
  pPrint :: a -> String

