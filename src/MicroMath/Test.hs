{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module MicroMath.Test where

import Data.Sequence (pattern (:<|), pattern Empty)
import Data.Text     (Text)
import Data.Text     qualified as Text
import MicroMath

builtinPlusRule :: Rule
builtinPlusRule = BuiltinRule $ \case
  Plus :@ (ExprInteger i :<| ExprInteger j :<| Empty) -> Just (ExprInteger (i+j))
  _ -> Nothing

builtinTimesRule :: Rule
builtinTimesRule = BuiltinRule $ \case
  Times :@ (ExprInteger i :<| ExprInteger j :<| Empty) -> Just (ExprInteger (i*j))
  _ -> Nothing

parseDeclarations :: Text -> [(Pat, Expr)]
parseDeclarations programText =
  maybe (error "Couldn't parse declarations") id $ do
  exprs <- parseProgramText programText
  mapM setDelayedFromExpr exprs

myDecls :: [(Pat, Expr)]
myDecls = parseDeclarations $ Text.unlines
  [ "square[x_] := x*x;"
  , "fib[0] := 0;"
  , "fib[1] := 1;"
  , "fib[n_] := fib[n-1] + fib[n-2];"
  , "main := fib[10];"
  ]

myContext :: Context
myContext = createContext $ do
  setAttributes "Plus"  $ emptyAttributes { flat = True, orderless = True }
  setAttributes "Times" $ emptyAttributes { flat = True, orderless = True }
  addDownValue "Plus"  builtinPlusRule
  addDownValue "Times" builtinTimesRule
  mapM_ (uncurry addPatRule) myDecls
