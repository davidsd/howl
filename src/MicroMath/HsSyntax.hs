{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

{- | HsSyntax

This module defines several ad-hoc operators and values to enable
construction of Expr's and Pat's in Haskell in a way that vaguely
mimics Mathematica syntax. For example, where in Mathematica we would have written

f[x_, y___] := x+9

In Haskell we can define the Rule

"f"![v"x", "y".:___] := "x" + 9

-}

module MicroMath.HsSyntax where

import MicroMath (ContextM, Expr (..), Pat (..), Rule (..), SeqType (..),
                  Symbol (..), addNames, addPatRule)

class HasApp a where
  (!) :: a -> [a] -> a
infixl 9 !

instance HasApp Expr where
  (!) = ExprApp

instance HasApp Pat where
  (!) = PatApp []

(./) :: Pat -> Expr -> Pat
p ./ test = PatCondition [] p test
infixl 6 ./

(.|) :: Pat -> Pat -> Pat
p1 .| p2 = PatAlt [] p1 p2
infixl 5 .|

(.:) :: Symbol -> Pat -> Pat
s .: p = addNames [s] p
infixr 8 .:

(.=) :: Pat -> Expr -> ContextM ()
(.=) = addPatRule
infixl 1 .=

pattern (:=) :: Pat -> Expr -> Rule
pattern p := e = PatRule p e
infixr 1 :=

v :: Symbol -> Pat
v name = PatVar (if name == "" then [] else [name]) Nothing

__ :: Pat
__ = PatSeqVar [] OneOrMore

___ :: Pat
___ = PatSeqVar [] ZeroOrMore
