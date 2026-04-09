{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}

module Main (main) where

import Control.Monad.IO.Class   (liftIO)
import Control.Monad.Trans      (lift)
import Data.List                qualified as List
import Data.Text                (Text)
import Data.Text                qualified as Text
import Howl                     (Eval, Expr (..), PPrint (..), compilePat,
                                 defStdLib, eval, fullForm, get, pattern Null,
                                 run, runEval)
import Howl.Parser              (parseExprText)
import Howl.Util                (pattern Solo)
import Options.Applicative
import System.Console.Haskeline (InputT, defaultSettings, getInputLine,
                                 outputStrLn, runInputT)

data Options = MkOptions
  { file :: !(Maybe FilePath)
  , expr :: !(Maybe Text.Text)
  }

optionsParser :: Parser Options
optionsParser =
  MkOptions
    <$> optional
      ( strOption
        ( long "file"
          <> short 'f'
          <> metavar "FILE"
          <> help "Evaluate FILE before optionally evaluating EXPR"
        )
      )
    <*> optional
      ( Text.pack <$> strOption
        ( long "expr"
          <> short 'e'
          <> metavar "EXPR"
          <> help "Evaluate EXPR"
        )
      )

optsInfo :: ParserInfo Options
optsInfo =
  info (optionsParser <**> helper)
  ( fullDesc
    <> progDesc "Howl REPL / evaluator"
    <> header "howl"
  )

main :: IO ()
main = do
  opts <- execParser optsInfo
  runEval $
    case (opts.file, opts.expr) of
      (Nothing, Nothing) -> runRepl
      _                  -> runBatch opts

runBatch :: Options -> Eval ()
runBatch opts = do
  defStdLib
  maybe (pure ()) runFileContent opts.file
  maybe (pure ()) runExprContent opts.expr
  where
    runFileContent :: FilePath -> Eval ()
    runFileContent fp = do
      inputExpr <- get fp
      result <- eval inputExpr
      unlessNull result $ liftIO . putStrLn . formatOutput

    runExprContent :: Text -> Eval ()
    runExprContent input = do
      result <- run input
      unlessNull result $ liftIO . putStrLn . formatOutput

runRepl :: Eval ()
runRepl = runInputT defaultSettings evalRepl

evalRepl :: InputT Eval ()
evalRepl = do
  outputStrLn "Howl, version 0.1 :? for help"
  lift defStdLib
  loop
  where
    loop :: InputT Eval ()
    loop = do
      maybeInput <- getInputLine "> "
      case maybeInput of
        Nothing -> pure ()
        Just ":?" -> do
          showHelp
          loop
        Just ":quit" -> pure ()
        Just input | ":showPat " `List.isPrefixOf` input -> do
          let patText = Text.pack (drop 9 input)
          case parseExprText patText of
            Left err -> outputStrLn err
            Right expr -> do
              pat <- lift $ compilePat expr
              outputStrLn (show pat)
          loop
        Just input | ":pat " `List.isPrefixOf` input -> do
          let patText = Text.pack (drop 5 input)
          case parseExprText patText of
            Left err -> outputStrLn err
            Right expr -> do
              pat <- lift $ compilePat expr
              outputStrLn (pPrint pat)
          loop
        Just input -> do
          result <- lift $ run (Text.pack input)
          unlessNull result $ outputStrLn . formatOutput
          loop

showHelp :: InputT Eval ()
showHelp = outputStrLn $ unlines
  [ ":quit            : Exit"
  , "?x (or Help[x])  : Print the SymbolRecord for the symbol x. Example: ?Expand."
  , "DefinedSymbols[] : A list of all symbols with a nontrivial SymbolRecord"
  , ":pat expr        : Compile expr as a pattern and print it with pPrint"
  , ":showPat expr    : Compile expr as a pattern and print it with show"
  , "FullForm[expr]   : Print the evaluated expression in full form"
  , "ShowForm[expr]   : Print the evaluated expression with Haskell's show"
  ]

unlessNull :: Monad m => Expr -> (Expr -> m ()) -> m ()
unlessNull Null _  = pure ()
unlessNull expr go = go expr

formatOutput :: Expr -> String
formatOutput expr = case expr of
  ExprApp "FullForm" (Solo e) -> fullForm e
  ExprApp "ShowForm" (Solo e) -> show e
  _                           -> pPrint expr
