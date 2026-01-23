{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}

module MicroMath.Eval.Context
  ( Rule(..)
  , Context(..)
  , Attributes(..)
  , HoldType(..)
  , emptyAttributes
  , SymbolRecord(..)
  , Eval(..)
  , runEvalWithContext
  , runEval
  , getContext
  , returnIfInCache
  , addToEvalCache
  , Decl(..)
  , newContext
  , lookupSymbolRecord
  , lookupAttributes
  , addDownValue
  , addUpValue
  , addDecl
  , modifyAttributes
  , setAttributes
  , setFlat
  , setOrderless
  , setNumericFunction
  , setHoldType
  , clear
  , clearAll
  , newModuleSymbol
  , getDefinedSymbols
  ) where

import Control.Monad.Catch      (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class   (MonadIO, liftIO)
import Control.Monad.Reader     (MonadReader, ReaderT, ask, runReaderT)
import Data.HashTable.IO        qualified as HT
import Data.IORef               (IORef, atomicModifyIORef', newIORef)
import Data.Sequence            (Seq, pattern Empty, (|>))
import Data.Sequence            qualified as Seq
import Data.Text.Short          qualified as ShortText
import MicroMath.Expr           (Expr (..))
import MicroMath.Eval.EvalCache (EvalCache, insertEvalCache, lookupEvalCache,
                                 newEvalCache)
import MicroMath.Pat            (Pat (..))
import MicroMath.PPrint         (PPrint (..))
import MicroMath.Symbol         (Symbol, symbolFromShortText, symbolToShortText)


type HashTable k v = HT.BasicHashTable k v

data Context = MkContext
  { symbolRecordTable      :: !(HashTable Symbol SymbolRecord)
  , moduleNumberRef        :: !(IORef Int)
  , addToEvalCacheHandler  :: Expr -> Eval ()
  , returnIfInCacheHandler :: Expr -> Eval Expr -> Eval Expr
  }

newtype Eval a = Eval (ReaderT Context IO a)
  deriving newtype (Functor, Applicative, Monad, MonadReader Context, MonadIO, MonadMask, MonadCatch, MonadThrow)

newContext :: IO Context
newContext = do
  symbolRecordTable <- HT.new
  moduleNumberRef <- newIORef 0
  evalCache <- newEvalCache
  pure $ MkContext
    { symbolRecordTable      = symbolRecordTable
    , moduleNumberRef        = moduleNumberRef
    , addToEvalCacheHandler  = defaultAddToEvalCache evalCache
    , returnIfInCacheHandler = defaultReturnIfInCache evalCache
    }

runEvalWithContext :: Context -> Eval a -> IO a
runEvalWithContext ctx (Eval f) = runReaderT f ctx

runEval :: Eval a -> IO a
runEval go = do
  ctx <- newContext
  runEvalWithContext ctx go

getContext :: Eval Context
getContext = ask

{-
-- To Turn off the eval cache, replace defaultAddToEvalCache with
-- dummyAddToEvalCache and defaultReturnIfInCache with
-- dummyReturnIfInCache. TODO: Make this user-configurable.

dummyAddToEvalCache :: EvalCache -> Expr -> Eval ()
dummyAddToEvalCache _ _ = pure ()

dummyReturnIfInCache :: EvalCache -> Expr -> Eval Expr -> Eval Expr
dummyReturnIfInCache _ _ go = go
-}

defaultAddToEvalCache :: EvalCache -> Expr -> Eval ()
defaultAddToEvalCache evalCache expr = liftIO $ insertEvalCache evalCache expr

defaultReturnIfInCache :: EvalCache -> Expr -> Eval Expr -> Eval Expr
defaultReturnIfInCache evalCache expr go =
  liftIO (lookupEvalCache evalCache expr) >>= \case
  True -> pure expr
  False -> go

returnIfInCache :: Expr -> Eval Expr -> Eval Expr
returnIfInCache expr go = do
  ctx <- getContext
  ctx.returnIfInCacheHandler expr go

addToEvalCache :: Expr -> Eval ()
addToEvalCache expr = do
  ctx <- getContext
  ctx.addToEvalCacheHandler expr

data Rule
  = PatRule Pat Expr
  | BuiltinRule (Expr -> Eval (Maybe Expr))

instance Show Rule where
  show (PatRule p expr) = pPrint p ++ " := " ++ pPrint expr
  show (BuiltinRule _)  = "<BuiltinRule>"

instance PPrint Rule where
  pPrint = show

-- | TODO: Add Protected, NumericFunction, OneIdentity
data Attributes = MkAttributes
  { flat            :: !Bool
  , orderless       :: !Bool
  , numericFunction :: !Bool
  , holdType        :: !(Maybe HoldType)
  } deriving (Eq, Ord, Show)

data HoldType = HoldFirst | HoldRest | HoldAll
  deriving (Eq, Ord, Show)

pattern EmptyAttributes :: Attributes
pattern EmptyAttributes = MkAttributes False False False Nothing

emptyAttributes :: Attributes
emptyAttributes = EmptyAttributes

setFlat :: Attributes -> Attributes
setFlat attr = attr { flat = True }

setOrderless :: Attributes -> Attributes
setOrderless attr = attr { orderless = True }

setNumericFunction :: Attributes -> Attributes
setNumericFunction attr = attr { numericFunction = True }

setHoldType :: HoldType -> Attributes -> Attributes
setHoldType ty attr = attr { holdType = Just ty }

-- | Information associated with a symbol, needed for evaluation.
--
-- NB: It is important that we store the associated Symbol in the
-- SymbolRecord because we are using 'symbolIndex's as keys to the
-- Context map, not Symbol's themselves. As a consequence, there is a
-- danger that a Symbol might get garbage collected, which would
-- result in a new symbol being created (with a different symbolIndex)
-- next time the same name is used. By storing the Symbol in the
-- SymbolRecord, we ensure that it won't get garbage collected as long
-- as a pointer to the SymbolRecord exists.
--
data SymbolRecord = MkSymbolRecord
  { -- symbol     :: Symbol
    ownValue   :: !(Maybe Expr)
  , downValues :: !(Seq Rule) -- ^ A rule that matches expressions where
                              -- the given symbol is the head
  , upValues   :: !(Seq Rule) -- ^ A rule that matches expressions where
                              -- the given symbol is 1 level below the
                              -- head.
  , attributes :: !Attributes
  }
  deriving (Show)

emptySymbolRecord :: Symbol -> SymbolRecord
emptySymbolRecord _ = MkSymbolRecord Nothing Seq.empty Seq.empty EmptyAttributes

modifyRecordOwnValue :: (Maybe Expr -> Maybe Expr) -> SymbolRecord -> SymbolRecord
modifyRecordOwnValue f record = record { ownValue = f record.ownValue }

modifyRecordDownValues :: (Seq Rule -> Seq Rule) -> SymbolRecord -> SymbolRecord
modifyRecordDownValues f record = record { downValues = f record.downValues }

modifyRecordUpValues :: (Seq Rule -> Seq Rule) -> SymbolRecord -> SymbolRecord
modifyRecordUpValues f record = record { upValues = f record.upValues }

modifyRecordAttributes :: (Attributes -> Attributes) -> SymbolRecord -> SymbolRecord
modifyRecordAttributes f record = record { attributes = f record.attributes }

-- | Apply the given function to a record, and remove the record from
-- the map if it is empty.
modifyRecord :: Symbol -> (SymbolRecord -> SymbolRecord) -> Eval ()
modifyRecord sym f = do
  ctx <- getContext
  let withUnit x = (x, ())
  liftIO $ HT.mutate ctx.symbolRecordTable sym $ withUnit . \case
    Nothing -> Just $ f (emptySymbolRecord sym)
    Just record ->
      let
        newRecord = f record
      in
        if isEmpty newRecord
        then Nothing
        else Just newRecord
  where
    --isEmpty (MkSymbolRecord _ Nothing Empty Empty EmptyAttributes) = True
    isEmpty (MkSymbolRecord Nothing Empty Empty EmptyAttributes) = True
    isEmpty _                                                    = False

lookupSymbolRecord :: Symbol -> Eval (Maybe SymbolRecord)
lookupSymbolRecord sym = do
  ctx <- getContext
  liftIO $ HT.lookup ctx.symbolRecordTable sym

lookupSymbolRecordDefault :: Symbol -> Eval SymbolRecord
lookupSymbolRecordDefault sym =
  fmap (maybe (emptySymbolRecord sym) id) $ lookupSymbolRecord sym

lookupAttributes :: Symbol -> Eval Attributes
lookupAttributes = fmap (.attributes) . lookupSymbolRecordDefault

modifyOwnValue :: Symbol -> (Maybe Expr -> Maybe Expr) -> Eval ()
modifyOwnValue sym f = modifyRecord sym (modifyRecordOwnValue f)

modifyDownValues :: Symbol -> (Seq Rule -> Seq Rule) -> Eval ()
modifyDownValues sym f = modifyRecord sym (modifyRecordDownValues f)

modifyUpValues :: Symbol -> (Seq Rule -> Seq Rule) -> Eval ()
modifyUpValues sym f = modifyRecord sym (modifyRecordUpValues f)

addDownValue :: Symbol -> Rule -> Eval ()
addDownValue sym rule = modifyDownValues sym (|> rule)

addUpValue :: Symbol -> Rule -> Eval ()
addUpValue sym rule = modifyUpValues sym (|> rule)

data Decl
  = OwnValue Symbol Expr
  | DownValue Symbol Rule
  | UpValue Symbol Rule
  deriving (Show)

addDecl :: Decl -> Eval ()
addDecl = \case
  OwnValue  sym expr -> modifyOwnValue sym (const (Just expr))
  DownValue sym rule -> addDownValue   sym rule
  UpValue   sym rule -> addUpValue     sym rule

modifyAttributes :: Symbol -> (Attributes -> Attributes) -> Eval ()
modifyAttributes sym f = modifyRecord sym (modifyRecordAttributes f)

setAttributes :: Symbol -> Attributes -> Eval ()
setAttributes sym attrs = modifyAttributes sym (const attrs)

clear :: Symbol -> Eval ()
clear sym = modifyRecord sym $
  modifyRecordOwnValue (const Nothing) .
  modifyRecordDownValues (const Seq.empty) .
  modifyRecordUpValues (const Seq.empty)

clearAll :: Symbol -> Eval ()
clearAll sym = modifyRecord sym (const (emptySymbolRecord sym))

newModuleSymbol :: Symbol -> Eval Symbol
newModuleSymbol x = do
  ctx <- ask
  n   <- liftIO $ atomicModifyIORef' ctx.moduleNumberRef $ \n -> (n+1,n)
  pure $
    symbolFromShortText $
    symbolToShortText x <> ShortText.pack ("$" <> show n)

getDefinedSymbols :: Eval [Symbol]
getDefinedSymbols = do
  ctx <- ask
  liftIO $ 
    fmap (map fst) $ HT.toList ctx.symbolRecordTable
