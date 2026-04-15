{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Howl.StdLib where

import Data.FileEmbed         (embedFile, makeRelativeToProject)
import Data.Foldable          qualified as Foldable
import Data.Sequence          (Seq, pattern (:<|), pattern (:|>), pattern Empty)
import Data.Sequence          qualified as Seq
import Data.Set               qualified as Set
import Data.String            (fromString)
import Data.Text              (Text)
import Data.Text              qualified as Text
import Data.Text.Encoding     qualified as TE
import Howl.Eval.Context      (Eval)
import Howl.Expr              (Expr (..), Numeric (..),
                               pattern ExprRational)
import Howl.Expr.PPrint       ()
import Howl.StdLib.Algebra    (addAlgebraBuiltins)
import Howl.StdLib.Context    (addContextBuiltins, run_)
import Howl.StdLib.Data       (addDataBuiltins)
import Howl.StdLib.Logic      (addLogicBuiltins)
import Howl.StdLib.Numeric    (addNumericBuiltins)
import Howl.StdLib.Rules      (addRulesBuiltins)
import Howl.StdLib.Scoped     (addScopedBuiltins)

stdLibWL :: Text
stdLibWL =
  TE.decodeUtf8
  $(makeRelativeToProject "wl/StdLib.wl" >>= embedFile)

defStdLib :: Eval ()
defStdLib = do
  addContextBuiltins
  addScopedBuiltins
  addLogicBuiltins
  addRulesBuiltins
  addDataBuiltins
  addAlgebraBuiltins
  addNumericBuiltins
  run_ stdLibWL
