# MicroMath

MicroMath is an implementation of a microscopic subset of the [Wolfram Language](https://www.wolfram.com/language/) (which powers Mathematica), in Haskell. It is both a Haskell library and executable. As a Haskell library, MicroMath makes it easy to define replacement rules using Haskell functions, and thereby use Haskell to manipulate algebraic expressions.  One can also define replacement rules using the usual Wolfram Language syntax, or a mixture of both languages, see [StdLib.hs](https://github.com/davidsd/micromath/blob/main/src/MicroMath/StdLib.hs) as an example.

## Intro: trees and replacement rules

At its core, Mathematica is an engine for repeatedly applying replacement rules to trees of data. A mathematical expression is represented as a tree. For example, the expression `3+a(b+c)` can be written `Plus[3,Times[a,Plus[b,c]]]`, which as a tree looks like this:

<img width="339" height="398" alt="Screenshot 2026-01-22 at 11 59 16 PM" src="https://github.com/user-attachments/assets/941c3f90-6b87-4fc0-88f5-1d22714c681e" />

`Plus`, `Times`, `a`,`b`, and `c` are all symbols, while 3 is an integer literal.

Replacement rules have a pattern on the left-hand-side and an expression on the right-hand-side. For example, we might define the rule

```mathematica
(* Distributive property *)
Times[x_,Plus[y_,z_]] := Plus[Times[x,y],Times[x,z]];
```

Alternatively, we can write it using some syntactic sugar as

```mathematica
(* Distributive property again *)
x_(y_+z_) := x y+x z;
```

If we add this to the global rules, then Mathematica will recognize that part of the tree above matches the left-hand side of this rule with the substitutions `{x -> a, y -> b, z -> c}`. It will then replace that part of the tree with the right-hand side of the rule, with the given substitutions, giving in this case `3+(a b+a c)`. Mathematica also knows that `Plus` is "Flat", i.e. associative, so it will further simplify this expression to `3+a b+a c`, which as a tree looks like this

<img width="338" height="266" alt="Screenshot 2026-01-23 at 12 04 15 AM" src="https://github.com/user-attachments/assets/db9453b4-fed1-4c36-a658-dc3a329d0596" />

In actuality, this example doesn't work in Mathematica because the symbol `Times` is protected and you are not allowed to define new rules for it. But you can do it in MicroMath:

```
MicroMath, version 0.1 :? for help
> x_(y_+z_) := x y+x z;
> 3+a(b+c)
Plus[3, Times[a, b], Times[a, c]]
```

## Matching algorithm and Loris

The core algorithm needed to implement the Wolfram Language is a procedure for matching patterns to expressions. MicroMath implements the algorithm $M_\textrm{Mma}$ described in [Variadic equational matching in associative and commutative theories](https://www.sciencedirect.com/science/article/pii/S0747717121000079) by Besik Dundua, Temur Kutsia, and Mircea Marin
[pdf](https://www3.risc.jku.at/publications/download/risc_6260/variadic-equational-matching-jsc-final-with-mma-versions.pdf). 

[loris](https://github.com/rljacobson/loris) is another implementation (in Rust) of the Wolfram Language based on this paper. Loris was important inspiration for MicroMath.

## Interoperation with Haskell

When MicroMath is used as a Haskell library, you can easily define replacement rules that use Haskell functions.
```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import MicroMath

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fib :: Int -> Integer
fib n = fibs !! n

myProgram :: Eval Expr
myProgram = do
  defStdLib
  def "Fib" fib
  run "Expand[(x + Fib[100])^Fib[3]]"

-- Prints: Plus[125475243067621153271396401396356512255625, Power[x, 2], Times[708449696358523830150, x]]
main :: IO ()
main = runEval myProgram >>= print
```
The type of the function `fib :: Int -> Integer` is used to define a rule that only matches expressions of the form `Fib[n]` where `n` is an integer literal, and returns an integer literal. For example, `Fib[x]` (where x is a symbol) doesn't match the rule we defined, and remains `Fib[x]`. The typeclasses `ToExpr`/`FromExpr` are used to automatically convert `Expr`'s to and from Haskell data, and define rules that only match `Expr`'s of the appropriate form.

Why would you want to do this? Well, it is generally horrible to write actual programs in Mathematica. It does not have a type system, it is slow, lists are the only conveniently available data structure, editing interfaces are bad. So instead, you can write your programs in Haskell. But Haskell does not have much in the way of computer algebra. So when you need mathematical expressions and simplification using replacement rules, you can use a `MicroMath` `Expr`.

Often in theoretical physics, we encounter a need to define custom symbolic manipulation rules. Some examples are Clifford matrix algebra used in Feynman diagrams, algebras of creation and annihilation operators, or vector calculus. The Wolfram Language was designed (in part) to make it easy to create these custom systems. That is an application where it really shines. MicroMath makes it possible to use these customized algebraic systems in Haskell.

## Some differences from Mathematica

Here is a woefully incomplete list of differences between MicroMath and Mathematica

- MicroMath has a few dozen functions in the standard library. Mathematica version 14.0 has 6,600 builtin functions.
- MicroMath is slower than Mathematica. How much slower depends on the program. For example, the following program takes 40 seconds to run in MicroMath and 7 seconds in Mathematica, on my M4 Max laptop:
  ```mathematica
  fib[0] := 0;
  fib[1] := 1;
  fib[n_] := fib[n-1] + fib[n-2];
  fib[35]
  ```
  If you know how to make it faster, please tell me!
- MicroMath does not evaluate patterns as if they were expressions. In other words, you can imagine that every pattern in MicroMath is wrapped in `HoldPattern`.
- MicroMath does not currently attempt to sort user-defined rules in reverse order of specificity. It stores rules in the order that they are defined. For example:
  ```mathematica
  Foo[_] := True;
  Foo[3] := False;
  (* Evaluates to False in Mathematica, but True in MicroMath *)
  Foo[3]
  ```
- MicroMath does not have `Block` or `With`. Instead, it defines `Let`, which implements shadowing, as is standard in most functional languages. For example:
  ```
  > Let[x=12,Let[x=9,x+3]+x]
  24
  ```
  Internally, `Let` is defined in terms of `Function`: `Let[x=a,expr] --> Function[x,expr][a]`. `Let` also lets you sanely define successive variables that depend on previous ones:
  ```
  > Let[{x=1,y=x+2,z=x+y+3}, z+4]
  11
  ```
- MicroMath implements the attributes `Flat`, `Orderless`, `HoldAll`, `HoldFirst`, and `HoldRest`. It does not (yet) have the attribute `OneIdentity`. It also does not yet implement `Evaluate` and `Unevaluated`.
- Attributes must be set before rules are defined. The reason is that the left-hand-side is compiled into a pattern, and the way compilation works depends on the attributes of the symbols in the pattern. If these attributes are changed later, the compiled pattern that is already in the Context will not be updated.
- MicroMath does not match subexpressions under a `Flat` `Orderless` in the same way as Mathematica. For example, in Mathematica, you can do
  ```
  > a b c /. {a c :> Foobar}
  b Foobar
  ```
  MicroMath does not recognize that `Times[a,b,c]` can be rewritten as `Times[Times[a,c],b]` which has `Times[a,c]` as a sub-expression that matches the pattern. However, you can do this:
  ```
  > a b c /. {a c x___ :> Foobar x}
  b Foobar
  ```
  This works because the left-hand side matches the whole expression `a b c`, taking into account commutativity of `Times` (which the MicroMath pattern matcher does).
  






