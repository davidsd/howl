{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoFieldSelectors  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms   #-}

module Main (main) where

import Data.Text                   (Text)
import Data.Text                   qualified as Text
import Data.Text.IO                qualified as Text
import Howl                        (Context, Expr, PPrint (..), defStdLib, eval,
                                    incrLineNumber, newContext, parseExprText,
                                    pattern Null,
                                    runEvalWithContext)
import Howl.Eval.Context           (setErrorLineHandler,
                                    setOutputLineHandler)
import IHaskell.IPython.EasyKernel (KernelConfig (..), easyKernel)
import IHaskell.IPython.Types      (DisplayData (..), ExecuteReplyStatus (..),
                                    KernelSpec (..), LanguageInfo (..),
                                    MimeType (..))
import System.Directory            (createDirectoryIfMissing,
                                    )
import System.Environment          (getArgs, getExecutablePath)
import System.FilePath             ((</>))
import System.Process              (callProcess)

data KernelState = MkKernelState
  { kernelContext :: !Context
  }

data HowlOutput
  = HowlOutput Text
  | HowlErrorOutput Text

data HowlResult
  = HowlNoResult
  | HowlSuccess Expr
  | HowlFailure Text

main :: IO ()
main = do
  args <- getArgs
  exePath <- getExecutablePath
  kernelState <- initKernelState
  let config = howlKernelConfig exePath kernelState
  case args of
    ["install"]      -> installKernelSpec exePath
    [connectionFile] -> easyKernel connectionFile config
    _                -> putStrLn "usage: howl-jupyter install | howl-jupyter <connection-file>"

initKernelState :: IO KernelState
initKernelState = do
  ctx <- newContext
  runEvalWithContext ctx defStdLib
  pure $ MkKernelState ctx

installKernelSpec :: FilePath -> IO ()
installKernelSpec exePath = do
  let kernelDir = ".stack-work" </> "howl-jupyter-kernelspec"
  createDirectoryIfMissing True kernelDir
  Text.writeFile (kernelDir </> "kernel.json") (kernelJson exePath)
  callProcess "jupyter" ["kernelspec", "install", "--user", "--name", "howl", "--replace", kernelDir]

kernelJson :: FilePath -> Text
kernelJson exePath = Text.unlines
  [ "{"
  , "  \"argv\": ["
  , "    " <> jsonString exePath <> ","
  , "    \"{connection_file}\""
  , "  ],"
  , "  \"display_name\": \"Howl\","
  , "  \"language\": \"howl\""
  , "}"
  ]

jsonString :: String -> Text
jsonString s =
  "\"" <> Text.concatMap escapeChar (Text.pack s) <> "\""
  where
    escapeChar '"'  = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar c    = Text.singleton c

howlKernelConfig :: FilePath -> KernelState -> KernelConfig IO HowlOutput HowlResult
howlKernelConfig exePath kernelState =
  KernelConfig
    { kernelLanguageInfo = LanguageInfo
        { languageName = "howl"
        , languageVersion = "0.1.0.0"
        , languageFileExtension = ".wl"
        , languageCodeMirrorMode = "mathematica"
        , languagePygmentsLexer = "mathematica"
        , languageMimeType = "text/plain"
        }
    , writeKernelspec = \_ ->
        pure KernelSpec
          { kernelDisplayName = "Howl"
          , kernelLanguage = "howl"
          , kernelCommand = [exePath, "{connection_file}"]
          }
    , displayOutput = \case
        HowlOutput txt -> [DisplayData PlainText txt]
        HowlErrorOutput txt ->
          [ DisplayData PlainText txt
          , DisplayData MimeHtml ("<pre style=\"color:#b00020;\">" <> txt <> "</pre>")
          ]
    , displayResult = \case
        HowlNoResult     -> []
        HowlSuccess expr -> [DisplayData PlainText (Text.pack (pPrint expr))]
        HowlFailure err  ->
          [ DisplayData PlainText err
          , DisplayData MimeHtml ("<pre style=\"color:#b00020;\">" <> err <> "</pre>")
          ]
    , completion = \_ _ -> pure ("", [])
    , inspectInfo = \_ _ -> pure Nothing
    , run = runHowlCell kernelState
    , debug = False
    , kernelBanner = "Howl Jupyter kernel"
    , kernelProtocolVersion = "5.3"
    , kernelImplName = "howl-jupyter"
    , kernelImplVersion = "0.1.0.0"
    }

runHowlCell
  :: KernelState
  -> Text
  -> IO ()
  -> (HowlOutput -> IO ())
  -> IO (HowlResult, ExecuteReplyStatus, String)
runHowlCell kernelState input clearOutput sendOutput =
  case parseExprText input of
    Left err ->
      pure (HowlFailure (Text.pack err), Err, "")
    Right expr -> do
      clearOutput
      setErrorLineHandler
        kernelState.kernelContext
        (sendOutput . HowlErrorOutput)
      setOutputLineHandler
        kernelState.kernelContext
        (sendOutput . HowlOutput)
      result <- runEvalWithContext kernelState.kernelContext $ do
        incrLineNumber
        eval expr
      pure
        ( if result == Null
          then HowlNoResult
          else HowlSuccess result
        , Ok
        , ""
        )
