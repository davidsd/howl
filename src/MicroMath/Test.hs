{-# LANGUAGE OverloadedStrings #-}

module MicroMath.Test where

import Data.Map.Strict    qualified as Map
import MicroMath
import MicroMath.HsSyntax

myExpr :: Expr
myExpr = "a"![12, "c", "d"![fromRational $ 17/2]]

myPat :: Pat
myPat = "y".:(v"x" ! [v"", "A".:"A1".:__, "B".:___])

myRHS :: Expr
myRHS = "y"!["A", "foo", "B"]

mySubstSet :: SubstitutionSet
mySubstSet = MkSubstitutionSet $
  Map.insert "c" ("Sequence"!["e", "f"]) Map.empty

myRule2 :: Rule
myRule2 = BuiltinRule f
  where
    f expr = case expr of
      (ExprApp "Plus"
        [ExprAtom (LitInteger i), ExprAtom (LitInteger j)]) -> Just (ExprAtom (LitInteger (i+j)))
      _ -> Nothing

myContext :: Context
myContext = createContext $ do
  setAttributes "Plus" [Flat, Orderless]
  setAttributes "Times" [Flat, Orderless]
  addDownValue "Plus" myRule2
  "f"![v"x"] .= "x" + 12
