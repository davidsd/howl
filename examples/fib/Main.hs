{-# LANGUAGE OverloadedStrings #-}

module Main where

import Howl

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)

myProgram :: Eval Expr
myProgram = do
  def "Fib" (fibs !!)
  run "Expand[(x + Fib[100])^Fib[3]]"

-- | Prints: 125475243067621153271396401396356512255625 + x^2 + 708449696358523830150 x
main :: IO ()
main = runEval myProgram >>= putStrLn . pPrint
