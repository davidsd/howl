{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module MicroMath.Expr.PPrint
  ( Prec(..)
  , pPrintPrec
  , pPrintLit
  ) where

import Data.Foldable             qualified as Foldable
import Data.List                 (intercalate)
import Data.Sequence             qualified as Seq
import MicroMath.Expr.Internal   (Expr (..), Literal (..), pattern ExprNumeric)
import MicroMath.Expr.Numeric    (Numeric (..))
import MicroMath.Expr.Syntax     (pattern Plus, pattern Times, pattern Power,
                                  pattern List, pattern Rule, pattern RuleDelayed,
                                  pattern Set, pattern SetDelayed,
                                  pattern Equal, pattern Unequal,
                                  pattern Less, pattern Greater,
                                  pattern LessEqual, pattern GreaterEqual,
                                  pattern SameQ, pattern UnsameQ,
                                  pattern And, pattern Or,
                                  pattern Alternatives)
import MicroMath.PPrint          (PPrint (..))
import Numeric.Rounded.Simple qualified as Rounded

-- | Precedence levels for pretty printing (higher = binds tighter)
data Prec
  = PrecTop        -- Top level, no parens needed
  | PrecSet        -- = :=
  | PrecRule       -- -> :>
  | PrecOr         -- ||
  | PrecAnd        -- &&
  | PrecCompare    -- == != < > <= >= === =!=
  | PrecAlt        -- |
  | PrecPlus       -- + -
  | PrecTimes      -- * /
  | PrecPower      -- ^
  | PrecAtom       -- Atoms, function calls
  deriving (Eq, Ord)

-- | Get the next higher precedence level (for associativity handling)
succPrec :: Prec -> Prec
succPrec PrecTop     = PrecSet
succPrec PrecSet     = PrecRule
succPrec PrecRule    = PrecOr
succPrec PrecOr      = PrecAnd
succPrec PrecAnd     = PrecCompare
succPrec PrecCompare = PrecAlt
succPrec PrecAlt     = PrecPlus
succPrec PrecPlus    = PrecTimes
succPrec PrecTimes   = PrecPower
succPrec PrecPower   = PrecAtom
succPrec PrecAtom    = PrecAtom

-- | Pretty print with precedence context
pPrintPrec :: Prec -> Expr -> String
pPrintPrec _ (ExprSymbol s) = pPrint s
pPrintPrec _ (ExprLit l) = pPrintLit l
pPrintPrec ctx (ExprApp f args) = case (f, Foldable.toList args) of
  -- List: {a, b, c}
  (List, xs) -> "{" <> intercalate ", " (map pPrint xs) <> "}"

  -- Rule: a -> b (right-associative)
  (Rule, [lhs, rhs]) ->
    parensIf (ctx > PrecRule) $
      pPrintPrec (succPrec PrecRule) lhs <> " -> " <> pPrintPrec PrecRule rhs

  -- RuleDelayed: a :> b (right-associative)
  (RuleDelayed, [lhs, rhs]) ->
    parensIf (ctx > PrecRule) $
      pPrintPrec (succPrec PrecRule) lhs <> " :> " <> pPrintPrec PrecRule rhs

  -- Set: a = b (right-associative)
  (Set, [lhs, rhs]) ->
    parensIf (ctx > PrecSet) $
      pPrintPrec (succPrec PrecSet) lhs <> " = " <> pPrintPrec PrecSet rhs

  -- SetDelayed: a := b (right-associative)
  (SetDelayed, [lhs, rhs]) ->
    parensIf (ctx > PrecSet) $
      pPrintPrec (succPrec PrecSet) lhs <> " := " <> pPrintPrec PrecSet rhs

  -- Comparison operators
  (Equal, [a, b]) -> pPrintBinop ctx PrecCompare " == " a b
  (Unequal, [a, b]) -> pPrintBinop ctx PrecCompare " != " a b
  (Less, [a, b]) -> pPrintBinop ctx PrecCompare " < " a b
  (Greater, [a, b]) -> pPrintBinop ctx PrecCompare " > " a b
  (LessEqual, [a, b]) -> pPrintBinop ctx PrecCompare " <= " a b
  (GreaterEqual, [a, b]) -> pPrintBinop ctx PrecCompare " >= " a b
  (SameQ, [a, b]) -> pPrintBinop ctx PrecCompare " === " a b
  (UnsameQ, [a, b]) -> pPrintBinop ctx PrecCompare " =!= " a b

  -- Logical operators
  (And, xs@(_:_:_)) ->
    parensIf (ctx > PrecAnd) $ intercalate " && " (map (pPrintPrec PrecAnd) xs)
  (Or, xs@(_:_:_)) ->
    parensIf (ctx > PrecOr) $ intercalate " || " (map (pPrintPrec PrecOr) xs)

  -- Alternatives: a | b | c
  (Alternatives, xs@(_:_:_)) ->
    parensIf (ctx > PrecAlt) $ intercalate " | " (map (pPrintPrec PrecAlt) xs)

  -- Plus: a + b - c + d (handling negative terms)
  (Plus, xs@(_:_:_)) ->
    parensIf (ctx > PrecPlus) $ pPrintPlus xs

  -- Times: handle negative coefficient specially, otherwise use juxtaposition
  (Times, xs@(_:_:_)) ->
    parensIf (ctx > PrecTimes) $ pPrintTimes xs

  -- Power: a^b (right-associative)
  (Power, [base, expo]) ->
    parensIf (ctx > PrecPower) $
      pPrintPrec (succPrec PrecPower) base <> "^" <> pPrintPrec PrecPower expo

  -- Default: f[a, b, c]
  _ -> mconcat
    [ pPrint f
    , "["
    , intercalate ", " (map pPrint (Foldable.toList args))
    , "]"
    ]
  where
    parensIf True  s = "(" <> s <> ")"
    parensIf False s = s

    -- Non-associative binary operator: both sides use stricter precedence
    pPrintBinop c p op a b =
      parensIf (c > p) $ pPrintPrec (succPrec p) a <> op <> pPrintPrec (succPrec p) b

    -- Pretty print a sum, using - for negative terms
    pPrintPlus [] = "0"
    pPrintPlus (x:xs) = pPrintTerm x <> concatMap pPrintSignedTerm xs
      where
        pPrintTerm = pPrintPrec PrecPlus
        pPrintSignedTerm t = case negatedTerm t of
          Just t' -> " - " <> pPrintTerm t'
          Nothing -> " + " <> pPrintTerm t

    -- Check if a term is negated, return the positive version if so
    negatedTerm (ExprApp Times (Foldable.toList -> (ExprNumeric n : rest)))
      | n < 0 = Just $ case (negateNumeric n, rest) of
          (NInteger 1, xs) -> mkTimes xs
          (n', xs)         -> mkTimes (ExprNumeric n' : xs)
    negatedTerm (ExprNumeric n) | n < 0 = Just (ExprNumeric (negateNumeric n))
    negatedTerm _ = Nothing

    mkTimes [x] = x
    mkTimes xs  = ExprApp Times (Seq.fromList xs)

    negateNumeric (NInteger n)  = NInteger (-n)
    negateNumeric (NRational r) = NRational (-r)
    negateNumeric (NReal d)     = NReal (-d)
    negateNumeric (NBigFloat d) = NBigFloat (Rounded.negate_ Rounded.TowardNearest (Rounded.precision d) d)

    -- Pretty print a product, using juxtaposition with spaces where needed
    pPrintTimes [] = "1"
    pPrintTimes [x] = pPrintPrec PrecTimes x
    pPrintTimes (ExprNumeric n : xs) | n < 0 = "-" <> pPrintTimes (ExprNumeric (negateNumeric n) : xs)
    pPrintTimes (x:xs) = pPrintPrec PrecTimes x <> pPrintTimesRest xs

    -- Print remaining factors, adding space before each
    pPrintTimesRest [] = ""
    pPrintTimesRest (x:xs) = " " <> pPrintPrec PrecTimes x <> pPrintTimesRest xs

-- | Pretty print a literal (defined here to avoid import cycle)
pPrintLit :: Literal -> String
pPrintLit (LitNumeric x) = pPrint x
pPrintLit (LitString s)  = show s

instance PPrint Expr where
  pPrint = pPrintPrec PrecTop
