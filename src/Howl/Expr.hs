-- | Expression types, conversions, syntax patterns, and pretty-printing.
--
-- This module exports the core APIs for constructing, inspecting, and
-- rendering Howl expressions. Compared to "Howl", it exposes a larger
-- collection of expression patterns and lower-level expression
-- utilities.
--
-- The expression patterns exported by this module are bidirectional
-- pattern synonyms for symbol expressions such as @Plus@, @List@, and
-- @Rule@. For example, you can construct expressions using them like:
--
-- - @Plus :\@ Seq.fromList [toExpr 1, "x"]@ represents the Wolfram
--   Language expression @1 + x@
-- - @Rule :\@ Seq.fromList ["x", toExpr 3]@ represents the Wolfram
--   Language expression @x -> 3@
module Howl.Expr
  ( module Exports
  ) where

import Howl.Expr.Internal as Exports
import Howl.Expr.Numeric  as Exports
import Howl.Expr.PPrint   as Exports
import Howl.Expr.Syntax   as Exports
