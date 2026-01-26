{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import MicroMath
import Data.FileEmbed (makeRelativeToProject, strToExp)

vectorCalculusSrc :: FilePath
vectorCalculusSrc = $(makeRelativeToProject "examples/wl/vector_calculus.wl" >>= strToExp)

-- | The Coulomb potential in dim-dimensions is 1/|x|^(dim-2), where
-- |x|=CenterDot[x,x]^(1/2). This program loads some basic vector
-- calculus rules and verifies that the Coulomb potential obeys
-- Laplace's equation.
checkLaplace :: Eval Expr
checkLaplace = do
  defStdLib
  get_ vectorCalculusSrc
  run "Expand[laplacian[x][CenterDot[x,x]^((2-dim)/2)]] == 0"

-- | Should print True
main :: IO ()
main = runEval checkLaplace >>= putStrLn . pPrint
