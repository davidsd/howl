{-# LANGUAGE PatternSynonyms #-}

-- | The main API for Howl.
--
-- This module provides routines for:
--
-- - running and evaluating expressions,
-- - defining builtin rules from Haskell functions,
-- - parsing and pretty-printing expressions,
-- - working with 'Expr's.
--
-- For additional expression patterns and lower-level expression
-- utilities, see "Howl.Expr".
module Howl
  (
    -- * Running Howl Programs
    Eval
  , runEval
  , eval

    -- * Defining Builtins From Haskell
  , ToBuiltin
  , Variadic(..)
  , def

    -- * Parsing and Printing
  , run
  , run_
  , get
  , get_
  , parseExprText
  , readExprFile
  , PPrint(..)

    -- * Core Expression Types
  , Expr(..)
  , Symbol
  , Literal(..)
  , Numeric(..)
  , BigFloat
  , FromExpr(..)
  , ToExpr(..)

    -- * Common Expression Patterns
  , pattern List
  , pattern Plus
  , pattern Times
  , pattern Power
  , pattern Rule
  , pattern RuleDelayed
  , pattern Null
  , pattern (:@)
  ) where

import Howl.Builtins
    ( runEval )
import Howl.Builtins.Context
    ( get, get_, run, run_ )
import Howl.Builtins.ToBuiltin ( Variadic(..), def, ToBuiltin )
import Howl.Eval ( eval )
import Howl.Eval.Context
    ( Eval )
import Howl.Expr
    ( Numeric(..),
      BigFloat,
      Expr(..),
      FromExpr(..),
      Literal(..),
      ToExpr(..),
      pattern List,
      pattern Plus,
      pattern Times,
      pattern Power,
      pattern Rule,
      pattern RuleDelayed,
      pattern Null,
      pattern (:@) )
import Howl.Parser ( parseExprText, readExprFile )
import Howl.PPrint ( PPrint(..) )
import Howl.Symbol ( Symbol )
