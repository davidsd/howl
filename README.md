# MicroMath

MicroMath is an implementation of a microscopic subset of the [Wolfram Language](https://www.wolfram.com/language/) (which powers Mathematica) in Haskell. It is both a Haskell library and executable. As a Haskell library, MicroMath makes it easy to define replacement rules using Haskell functions, and thereby use Haskell to manipulate algebraic expressions.  One can also define replacement rules using the usual Wolfram Language syntax, or a mixture of both languages.

## Intro: trees and replacement rules

At its core, Mathematica is an engine for repeatedly applying replacement rules to trees of data. A mathematical expression is represented as a tree. For example, the expression `3+a*(b+c)` can be written `Plus[3,Times[a,Plus[b,c]]]`, which as a tree looks like this:

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
x_*(y_+z_) := x*y+x*z;
```

If we add this to the global rules, then Mathematica will recognize that part of the tree above matches the left-hand side of this rule with the substitutions `{x -> a, y -> b, z -> c}`. It will then replace that part of the tree with the right-hand side of the rule, with the given substitutions, giving in this case `3+(a*b+a*c)`. Mathematica also knows that `Plus` is "Flat", i.e. associative, so it will further simplify this expression to `3+a*b+a*c`, which as a tree looks like this

<img width="338" height="266" alt="Screenshot 2026-01-23 at 12 04 15 AM" src="https://github.com/user-attachments/assets/db9453b4-fed1-4c36-a658-dc3a329d0596" />

In actuality, this example doesn't work in Mathematica because the symbol `Times` is protected and you are not allowed to define new rules for it. But you can do it in MicroMath:

```
MicroMath, version 0.1 :? for help
> x_*(y_+z_) := x*y+x*z;
> 3+a*(b+c)
Plus[3, Times[a, b], Times[a, c]]
```

## Matching algorithm and Loris

The core algorithm needed to implement the Wolfram Language is a procedure for matching patterns to expressions. MicroMath implements the algorithm $M_\textrm{Mma}$ described in [Variadic equational matching in associative and commutative theories](https://www.sciencedirect.com/science/article/pii/S0747717121000079) by Besik Dundua, Temur Kutsia, and Mircea Marin
[pdf](https://www3.risc.jku.at/publications/download/risc_6260/variadic-equational-matching-jsc-final-with-mma-versions.pdf). 

[loris](https://github.com/rljacobson/loris) is another implementation (in Rust) of the Wolfram Language based on this paper. Loris was important inspiration for MicroMath.

## Differences with Mathematica

Here is a woefully incomplete list of differences between MicroMath and Mathematica

- MicroMath has a few dozen functions in the standard library. Mathematica version 14.0 has 6,600 builtin functions.
- MicroMath is slower than Mathematica. How much slower depends on the program. For example, the following program takes 40 seconds to run in MicroMath and 7 seconds in Mathematica, on my M4 Max laptop:
  ```mathematica
  fib[0] := 0;
  fib[1] := 1;
  fib[n_] := fib[n-1] + fib[n-2];
  fib[35]
  ```
- The MicroMath parser (currently) does not recognize whitespace as multiplication. You have to use `*`.
- MicroMath does not evaluate patterns as if they were expressions. In other words, you can imagine that every pattern in MicroMath is wrapped in `HoldPattern`.
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
- MicroMath does not match subexpressions under a `Flat` `Orderless` in the same way as Mathematica. For example, in Mathematica, you can do
  ```
  > a*b*c /. {a*c :> Foobar}
  b*Foobar
  ```
  MicroMath does not recognize that `Times[a,b,c]` can be rewritten as `Times[Times[a,c],b]` which has `Times[a,c]` as a sub-expression that matches the pattern. However, you can do this:
  ```
  > a*b*c /. {Times[a,c,rest___] :> Times[Foobar,rest]}
  Times[Foobar, b]
  ```
  This works because the left-hand side matches the whole expression `a*b*c`, taking into account commutativity of `Times` (which the MicroMath pattern matcher does).
  






