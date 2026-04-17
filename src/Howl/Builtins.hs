{-# LANGUAGE TemplateHaskell #-}

-- | The standard builtin definitions and initialization helpers.
--
-- This module embeds the standard Wolfram-language prelude, installs
-- the standard builtin rules into a context, and provides convenience
-- functions for running 'Eval' computations with those builtins
-- loaded.
module Howl.Builtins where

import Data.FileEmbed        (embedFile, makeRelativeToProject)
import Data.Text             (Text)
import Data.Text.Encoding    qualified as TE
import Howl.Builtins.Algebra (addAlgebraBuiltins)
import Howl.Builtins.Context (addContextBuiltins, run_)
import Howl.Builtins.Data    (addDataBuiltins)
import Howl.Builtins.Logic   (addLogicBuiltins)
import Howl.Builtins.Numeric (addNumericBuiltins)
import Howl.Builtins.Rules   (addRulesBuiltins)
import Howl.Builtins.Scoped  (addScopedBuiltins)
import Howl.Eval.Context     (Context, Eval, newContext, runEvalNewContext,
                              runEvalWithContext)

-- | The embedded standard Wolfram-language prelude.
preludeWL :: Text
preludeWL =
  TE.decodeUtf8
  $(makeRelativeToProject "wl/Prelude.wl" >>= embedFile)

-- | Add the standard builtins and prelude definitions to the current context.
addBuiltins :: Eval ()
addBuiltins = do
  addContextBuiltins
  addScopedBuiltins
  addLogicBuiltins
  addRulesBuiltins
  addDataBuiltins
  addAlgebraBuiltins
  addNumericBuiltins
  run_ preludeWL

-- | Create a new context initialized with the standard builtins.
newContextWithBuiltins :: IO Context
newContextWithBuiltins = do
  ctx <- newContext
  runEvalWithContext ctx addBuiltins
  pure ctx

-- | Create a new context, initialize it with the standard builtins
-- using 'addBuiltins', and run the 'Eval' computation in that context.
runEval :: Eval a -> IO a
runEval go =
  runEvalNewContext $ do
    addBuiltins
    go
