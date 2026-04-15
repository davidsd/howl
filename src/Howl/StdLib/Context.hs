{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}

module Howl.StdLib.Context
  ( LHS (..)
  , addContextBuiltins
  , setDef
  , setDelayedDef
  , run
  , run_
  , get
  , get_
  , evalWithHistory
  ) where

import Control.Monad     (void)
import Data.Foldable     qualified as Foldable
import Data.Sequence     (Seq, pattern (:|>), pattern Empty)
import Data.Text         (Text)
import Data.Text         qualified as Text
import Howl.Eval         (eval)
import Howl.Eval.Context (Attributes (..), Decl (..), Eval, HoldType (..),
                          Rule (..), addDecl, compilePat,
                          emitErrorLine, emitOutputLine, getLineNumber,
                          incrLineNumber, lookupSymbolRecord,
                          modifyAttributes, clear, clearAll,
                          getDefinedSymbols, setFlat, setHoldType,
                          setOrderless)
import Howl.Expr         (Expr (..), FromExpr (..), pattern (:@),
                          pattern ExprInteger, pattern ExprString,
                          pattern ExprSymbol, pattern Null,
                          pattern TagSetDelayed)
import Howl.Expr         qualified as Expr
import Howl.Parser       (parseExprText, readExprFile)
import Howl.Pat          (patRootSymbol)
import Howl.PPrint       (PPrint (..))
import Howl.StdLib.Types (ListOrSolo (..))
import Howl.Symbol       (Symbol)
import Howl.ToBuiltin    (Variadic (..), def)
import Howl.Util         (pattern Pair, pattern Solo)

---------- SetDelayed and Set ----------

data LHS
  = LHSSymbol Symbol
  | LHSPat Expr
  | LHSTaggedPat Symbol Expr
  deriving (Show)

instance FromExpr LHS where
  fromExpr = \case
    ExprSymbol sym                                   -> Just $ LHSSymbol sym
    TagSetDelayed :@ (Pair (ExprSymbol sym) patExpr) -> Just $ LHSTaggedPat sym patExpr
    patExpr                                          -> Just $ LHSPat patExpr

setPairToDecl :: LHS -> Expr -> Eval (Maybe Decl)
setPairToDecl lhs rhs = case lhs of
  LHSSymbol sym -> pure $ Just $ OwnValue sym rhs
  LHSPat patExpr -> do
    pat <- compilePat patExpr
    case patRootSymbol pat of
      Just sym -> pure $ Just $ DownValue sym (PatRule pat rhs)
      Nothing  -> do
        emitErrorLine "Pattern on the left-hand side has no root symbol"
        pure Nothing
  LHSTaggedPat sym patExpr -> do
    pat <- compilePat patExpr
    pure $ Just $ UpValue sym (PatRule pat rhs)

setDef :: LHS -> Expr -> Eval Expr
setDef lhs rhs = do
  setPairToDecl lhs rhs >>= \case
    Just decl -> do
      addDecl decl
      pure rhs
    Nothing -> pure Null

setDelayedDef :: LHS -> Expr -> Eval ()
setDelayedDef lhs rhs = setPairToDecl lhs rhs >>= \case
  Just decl -> addDecl decl
  Nothing   -> pure ()

---------- CompoundExpression ----------

compoundExpression :: Seq Expr -> Expr
compoundExpression = \case
  Empty       -> Null
  _ :|> final -> final

---------- Attributes ----------

newtype AttrModifier = MkAttrModifier { getModifier :: Attributes -> Attributes }

instance FromExpr AttrModifier where
  fromExpr = fmap MkAttrModifier . \case
    "Flat"      -> Just setFlat
    "Orderless" -> Just setOrderless
    "HoldAll"   -> Just $ setHoldType HoldAll
    "HoldFirst" -> Just $ setHoldType HoldFirst
    "HoldRest"  -> Just $ setHoldType HoldRest
    _           -> Nothing

setAttributes :: Symbol -> ListOrSolo AttrModifier -> Eval ()
setAttributes sym (MkListOrSolo attrs) =
  mapM_ (modifyAttributes sym . (.getModifier)) attrs

---------- Help ----------

printDef :: Seq Expr -> Eval Expr
printDef exprs = do
  emitOutputLine . Text.concat . Foldable.toList $ fmap renderPrintExpr exprs
  pure Null
  where
    renderPrintExpr = \case
      ExprString s -> s
      expr         -> Text.pack (pPrint expr)

helpDef :: Symbol -> Eval ()
helpDef sym = do
  maybeRecord <- lookupSymbolRecord sym
  emitOutputLine . Text.pack $ show maybeRecord

---------- Building Contexts ----------

run :: Text -> Eval Expr
run input = case parseExprText input of
  Left err   -> emitErrorLine (Text.pack err) >> pure Expr.Null
  Right expr -> eval expr

run_ :: Text -> Eval ()
run_ = void . run

get :: FilePath -> Eval Expr
get path = readExprFile path >>= \case
  Left err   -> emitErrorLine (Text.pack err) >> pure Expr.Null
  Right expr -> eval expr

get_ :: FilePath -> Eval ()
get_ = void . get

evalWithHistory :: Expr -> Eval (Int, Expr)
evalWithHistory expr = do
  inputCount <- getLineNumber
  let inputIndexExpr = ExprApp "In" (Solo (ExprInteger (fromIntegral inputCount)))
  let outputIndexExpr = ExprApp "Out" (Solo (ExprInteger (fromIntegral inputCount)))
  setDelayedDef (LHSPat inputIndexExpr) expr
  evalResult <- eval (ExprApp "Set" (Pair outputIndexExpr expr))
  incrLineNumber
  pure (inputCount, evalResult)

---------- In / Out ----------

prevLineDef :: Symbol -> Eval Expr
prevLineDef sym = do
  n <- getLineNumber
  eval $ ExprApp (ExprSymbol sym) (Solo (ExprInteger (fromIntegral (n - 1))))

negativeLineDef :: Symbol -> Integer -> Eval (Maybe Expr)
negativeLineDef sym i
  | i < 0 = do
      n <- getLineNumber
      fmap Just $ eval $
        ExprApp (ExprSymbol sym) (Solo (ExprInteger (fromIntegral n + i)))
  | otherwise = pure Nothing

addContextBuiltins :: Eval ()
addContextBuiltins = do
  modifyAttributes "Set" (setHoldType HoldFirst)
  def "Set" setDef

  modifyAttributes "SetDelayed" (setHoldType HoldAll)
  def "SetDelayed" setDelayedDef

  modifyAttributes "Hold" (setHoldType HoldAll)

  def "CompoundExpression" (MkVariadic compoundExpression)
  def "Get" (get . Text.unpack)
  def "In" (prevLineDef "In")
  def "In" (negativeLineDef "In")
  def "Out" (prevLineDef "Out")
  def "Out" (negativeLineDef "Out")
  def "SetAttributes" setAttributes
  def "Print" (MkVariadic printDef)
  def "Help" helpDef
  def "Clear" clear
  def "ClearAll" clearAll
  def "DefinedSymbols" getDefinedSymbols
