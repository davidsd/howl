{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}

module Main (main) where

import Control.Monad.IO.Class   (liftIO)
import Control.Monad.Trans      (lift)
import Data.Text                (Text)
import Data.Text                qualified as Text
import MicroMath                (Eval, Expr (..), PPrint (..), defStdLib, eval,
                                 fullForm, get, pattern Null, run, runEval)
import MicroMath.Util            (pattern Solo)
import Options.Applicative
import System.Console.Haskeline (InputT, defaultSettings, getInputLine,
                                 outputStrLn, runInputT)

-- | Top-level mode of operation
data Mode
  = Repl
  | EvalFile FilePath
  | EvalExpr Text.Text

modeParser :: Parser Mode
modeParser =
  EvalFile <$> strOption
  ( long "file"
    <> short 'f'
    <> metavar "FILE"
    <> help "Evaluate FILE and print the result"
  )
  <|> EvalExpr . Text.pack <$> strOption
  ( long "expr"
    <> short 'e'
    <> metavar "EXPR"
    <> help "Evaluate EXPR and print the result"
  )
  <|> pure Repl

optsInfo :: ParserInfo Mode
optsInfo =
  info (modeParser <**> helper)
  ( fullDesc
    <> progDesc "MicroMath REPL / evaluator"
    <> header "micromath"
  )

main :: IO ()
main = do
  mode <- execParser optsInfo
  runEval $ case mode of
    Repl         -> runRepl
    EvalFile fp  -> runFile fp
    EvalExpr txt -> runExpr txt

runFile :: FilePath -> Eval ()
runFile fp = do
  defStdLib
  inputExpr <- get fp
  result <- eval inputExpr
  unlessNull result $ liftIO . putStrLn . formatOutput

runExpr :: Text -> Eval ()
runExpr input = do
  defStdLib
  result <- run input
  unlessNull result $ liftIO . putStrLn . formatOutput

runRepl :: Eval ()
runRepl = runInputT defaultSettings evalRepl

evalRepl :: InputT Eval ()
evalRepl = do
  outputStrLn "MicroMath, version 0.1 :? for help"
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
        Just input -> do
          result <- lift $ run (Text.pack input)
          unlessNull result $ outputStrLn . formatOutput
          loop

showHelp :: InputT Eval ()
showHelp = outputStrLn $ unlines
  [ ":quit            : Exit"
  , "?x (or Help[x])  : Print the SymbolRecord for the symbol x. Example: ?Expand."
  , "DefinedSymbols[] : A list of all symbols with a nontrivial SymbolRecord"
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
