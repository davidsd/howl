{-# LANGUAGE TemplateHaskell #-}

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

preludeWL :: Text
preludeWL =
  TE.decodeUtf8
  $(makeRelativeToProject "wl/Prelude.wl" >>= embedFile)

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

newContextWithBuiltins :: IO Context
newContextWithBuiltins = do
  ctx <- newContext
  runEvalWithContext ctx addBuiltins
  pure ctx

runEval :: Eval a -> IO a
runEval go =
  runEvalNewContext $ do
    addBuiltins
    go
