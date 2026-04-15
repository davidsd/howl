{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoFieldSelectors  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms   #-}

module Main (main) where

import Data.Aeson                  (ToJSON (..), encode, object, (.=))
import Data.ByteString.Lazy        qualified as BSL
import Data.Text                   (Text)
import Data.Text                   qualified as Text
import Howl                        (Context, Expr, PPrint (..), addBuiltins,
                                    evalWithHistory, newContext, parseExprText,
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
  runEvalWithContext ctx addBuiltins
  pure $ MkKernelState ctx

installKernelSpec :: FilePath -> IO ()
installKernelSpec exePath = do
  let kernelDir = ".stack-work" </> "howl-jupyter-kernelspec"
  createDirectoryIfMissing True kernelDir
  BSL.writeFile (kernelDir </> "kernel.json") (encode (kernelSpecFile exePath))
  callProcess "jupyter" ["kernelspec", "install", "--user", "--name", "howl", "--replace", kernelDir]

data KernelSpecFile = KernelSpecFile
  { argv        :: [String]
  , displayName :: Text
  , language    :: Text
  }

instance ToJSON KernelSpecFile where
  toJSON spec =
    object
      [ "argv" .= spec.argv
      , "display_name" .= spec.displayName
      , "language" .= spec.language
      ]

kernelSpecFile :: FilePath -> KernelSpecFile
kernelSpecFile exePath =
  KernelSpecFile
    { argv        = [exePath, "{connection_file}"]
    , displayName = "Howl"
    , language    = "howl"
    }

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
      (_, result) <- runEvalWithContext kernelState.kernelContext $
        evalWithHistory expr
      pure
        ( if result == Null
          then HowlNoResult
          else HowlSuccess result
        , Ok
        , ""
        )
