module MicroMath.Test where

import MicroMath

main :: IO ()
main = putStrLn "Test suite not yet implemented"

var :: Symbol -> Pat
var name = PatVar (if name == "" then [] else [name]) Nothing

myExpr :: Expr
myExpr = "a"![12, "c", "d"![fromRational $ 17/2]]

myPat :: Pat
myPat =
  addNames ["y"] $
  (var "x")!
  [ var ""
  , PatSeqVar ["A","A1"] OneOrMore
  , PatSeqVar ["B"] ZeroOrMore
  ]

myRHS :: Expr
myRHS = "y"!["A", "foo", "B"]

mySubstSet :: SubstitutionSet
mySubstSet = MkSubstitutionSet $
  Map.insert "c" ("Sequence"!["e", "f"]) Map.empty

myRule :: Rule
myRule = "f"![var "x"] := "x" + 12

myRule2 :: Rule
myRule2 = BuiltinRule f
  where
    f expr = case expr of
      (ExprApp "Plus"
        [ExprAtom (LitInteger i), ExprAtom (LitInteger j)]) -> Just (ExprAtom (LitInteger (i+j)))
      _ -> Nothing

myContext :: Context
myContext = MkContext $ Map.fromList
  [ ( "f"
    ,  MkSymbolRecord
      { ownValue = Nothing
      , downValues = [myRule]
      , upValues = []
      , attributes = Set.empty
      }
    )
  , ( "Plus"
    ,  MkSymbolRecord
      { ownValue = Nothing
      , downValues = [myRule2]
      , upValues = []
      , attributes = Set.fromList [Flat, Orderless]
      }
    )
  , ( "Times"
    ,  MkSymbolRecord
      { ownValue = Nothing
      , downValues = []
      , upValues = []
      , attributes = Set.fromList [Flat, Orderless]
      }
    )
  ]
