module MicroMath.PPrint where

import Symbolize (Symbol)
import Symbolize qualified as Symbolize

class PPrint a where
  pPrint :: a -> String

instance PPrint Symbol where
  pPrint = Symbolize.unintern
