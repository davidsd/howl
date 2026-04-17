{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}

-- | Core evaluation context types and operations.
--
-- This module defines the evaluator context, rules, declarations,
-- attributes, and lower-level operations for running and extending
-- Howl evaluation.
module Howl.Eval.Context
  ( Rule(..)
  , Context(..)
  , Attributes(..)
  , HoldType(..)
  , emptyAttributes
  , SymbolRecord(..)
  , DownValues(..)
  , Eval(..)
  , runEvalWithContext
  , runEvalNewContext
  , getContext
  , returnIfInCache
  , addToEvalCache
  , emitErrorLine
  , emitOutputLine
  , Decl(..)
  , newContext
  , getLineNumber
  , incrLineNumber
  , setErrorLineHandler
  , setOutputLineHandler
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
  , compilePat
  , dummyAddToEvalCache
  , dummyReturnIfInCache
  ) where

import Control.Monad.Catch    (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader   (MonadReader, ReaderT, ask, runReaderT)
import Data.HashTable.IO      qualified as HT
import Data.IORef             (IORef, atomicModifyIORef', newIORef, readIORef,
                               writeIORef)
import Data.List              (intercalate)
import Data.Map.Strict        (Map)
import Data.Map.Strict        qualified as Map
import Data.Sequence          (Seq, pattern Empty, (|>))
import Data.Sequence          qualified as Seq
import Data.Text              (Text)
import Data.Text              qualified as Text
import Data.Text.Short        qualified as ShortText
import Howl.Eval.EvalCache    (EvalCache, insertEvalCache, lookupEvalCache,
                               newEvalCache)
import Howl.Expr              (Expr (..))
import Howl.Pat               (Pat (..), PatAppType (..), matchesUniqueExpr,
                               patFromExpr)
import Howl.PPrint            (PPrint (..))
import Howl.Symbol            (Symbol, symbolFromShortText, symbolToShortText)


type HashTable k v = HT.BasicHashTable k v

-- | The mutable state used when evaluating expressions.
data Context = MkContext
  { symbolRecordTable      :: !(HashTable Symbol SymbolRecord)
  , moduleNumberRef        :: !(IORef Int)
  , lineNumberRef          :: !(IORef Int)
  , errorLineHandlerRef    :: !(IORef (Text -> IO ()))
  , outputLineHandlerRef   :: !(IORef (Text -> IO ()))
  , addToEvalCacheHandler  :: Expr -> Eval ()
  , returnIfInCacheHandler :: Expr -> Eval Expr -> Eval Expr
  }

-- | The monad in which Howl evaluation runs.
--
-- An @Eval@ computation has access to the current evaluation context,
-- including symbol definitions, attributes, line-number state, and
-- output/error handlers.
newtype Eval a = Eval (ReaderT Context IO a)
  deriving newtype (Functor, Applicative, Monad, MonadFail, MonadReader Context, MonadIO, MonadMask, MonadCatch, MonadThrow)

-- | Create a new empty evaluation context.
newContext :: IO Context
newContext = do
  symbolRecordTable <- HT.new
  moduleNumberRef <- newIORef 0
  lineNumberRef <- newIORef 1
  errorLineHandlerRef <- newIORef (putStrLn . Text.unpack)
  outputLineHandlerRef <- newIORef (putStrLn . Text.unpack)
  evalCache <- newEvalCache
  pure $ MkContext
    { symbolRecordTable      = symbolRecordTable
    , moduleNumberRef        = moduleNumberRef
    , lineNumberRef          = lineNumberRef
    , errorLineHandlerRef    = errorLineHandlerRef
    , outputLineHandlerRef   = outputLineHandlerRef
    , addToEvalCacheHandler  = defaultAddToEvalCache evalCache
    , returnIfInCacheHandler = defaultReturnIfInCache evalCache
    -- , addToEvalCacheHandler  = dummyAddToEvalCache evalCache
    -- , returnIfInCacheHandler = dummyReturnIfInCache evalCache
    }

-- | Run an @Eval@ computation in the given context.
runEvalWithContext :: Context -> Eval a -> IO a
runEvalWithContext ctx (Eval f) = runReaderT f ctx

-- | Create a new context and run an @Eval@ computation in it.
runEvalNewContext :: Eval a -> IO a
runEvalNewContext go = do
  ctx <- newContext
  runEvalWithContext ctx go

-- | Get the current context.
getContext :: Eval Context
getContext = ask

-- | Get the current input line number.
getLineNumber :: Eval Int
getLineNumber = do
  ctx <- getContext
  liftIO $ readIORef ctx.lineNumberRef

-- | Increment the current input line number.
incrLineNumber :: Eval ()
incrLineNumber = do
  ctx <- getContext
  liftIO $ atomicModifyIORef' ctx.lineNumberRef $ \n -> (n + 1, ())

-- To Turn off the eval cache, replace defaultAddToEvalCache with
-- dummyAddToEvalCache and defaultReturnIfInCache with
-- dummyReturnIfInCache. TODO: Make this user-configurable.

-- | A no-op cache insertion hook.
dummyAddToEvalCache :: EvalCache -> Expr -> Eval ()
dummyAddToEvalCache _ _ = pure ()

-- | A cache lookup hook that always evaluates the given computation.
dummyReturnIfInCache :: EvalCache -> Expr -> Eval Expr -> Eval Expr
dummyReturnIfInCache _ _ go = go

defaultAddToEvalCache :: EvalCache -> Expr -> Eval ()
defaultAddToEvalCache evalCache expr = liftIO $ insertEvalCache evalCache expr

defaultReturnIfInCache :: EvalCache -> Expr -> Eval Expr -> Eval Expr
defaultReturnIfInCache evalCache expr go =
  liftIO (lookupEvalCache evalCache expr) >>= \case
  True -> pure expr
  False -> go

-- | Return the cached value of an expression when available, otherwise
-- run the given computation.
returnIfInCache :: Expr -> Eval Expr -> Eval Expr
returnIfInCache expr go = do
  ctx <- getContext
  ctx.returnIfInCacheHandler expr go

-- | Add an expression to the evaluation cache.
addToEvalCache :: Expr -> Eval ()
addToEvalCache expr = do
  ctx <- getContext
  ctx.addToEvalCacheHandler expr

-- | Emit a line of error output using the current error handler.
emitErrorLine :: Text -> Eval ()
emitErrorLine line = do
  ctx <- getContext
  errorHandler <- liftIO $ readIORef ctx.errorLineHandlerRef
  liftIO $ errorHandler line

-- | Emit a line of standard output using the current output handler.
emitOutputLine :: Text -> Eval ()
emitOutputLine line = do
  ctx <- getContext
  outputHandler <- liftIO $ readIORef ctx.outputLineHandlerRef
  liftIO $ outputHandler line

-- | Set the error output handler for a 'Context'.
setErrorLineHandler :: Context -> (Text -> IO ()) -> IO ()
setErrorLineHandler ctx =
  writeIORef ctx.errorLineHandlerRef

-- | Set the standard output handler for a 'Context'.
setOutputLineHandler :: Context -> (Text -> IO ()) -> IO ()
setOutputLineHandler ctx =
  writeIORef ctx.outputLineHandlerRef

-- | A rewrite rule used by the evaluator.
data Rule
  = -- | A rule with a compiled pattern and right-hand side expression.
    PatRule Pat Expr
  | -- | A builtin rule implemented by a Haskell function.
    BuiltinRule (Expr -> Eval (Maybe Expr))

instance Show Rule where
  show (PatRule p expr) = pPrint p ++ " := " ++ pPrint expr
  show (BuiltinRule _)  = "<BuiltinRule>"

instance PPrint Rule where
  pPrint = show

-- | For cases when a pattern matches a unique expression, we store that
-- expression in the uniqueMatches Map for fast lookup. In the evaluator, we try
-- these lookups first before moving through the sequentialRules.
-- | The down-values associated with a symbol.
data DownValues = MkDownValues
  { sequentialRules :: !(Seq Rule)
  , uniqueMatches   :: !(Map Expr Expr)
  }

instance Show DownValues where
  show downVals = concat
    [ "MkDownValues { sequentialRules = "
    , show downVals.sequentialRules
    , ", uniqueMatches = fromList ["
    , intercalate ", " $
        map
          (\(lhs, rhs) -> "(" ++ pPrint lhs ++ ", " ++ pPrint rhs ++ ")")
          (Map.toList downVals.uniqueMatches)
    , "] }"
    ]

emptyDownValues :: DownValues
emptyDownValues = MkDownValues Seq.empty Map.empty

-- | TODO: Add Protected, NumericFunction, OneIdentity
-- | The attributes associated with a symbol.
data Attributes = MkAttributes
  { flat            :: !Bool
  , orderless       :: !Bool
  , numericFunction :: !Bool
  , holdType        :: !(Maybe HoldType)
  } deriving (Eq, Ord, Show)

-- | The hold behavior of a symbol during evaluation.
data HoldType = HoldFirst | HoldRest | HoldAll
  deriving (Eq, Ord, Show)

pattern EmptyAttributes :: Attributes
pattern EmptyAttributes = MkAttributes False False False Nothing

-- | The default empty set of symbol attributes.
emptyAttributes :: Attributes
emptyAttributes = EmptyAttributes

-- | Set the @Flat@ attribute.
setFlat :: Attributes -> Attributes
setFlat attr = attr { flat = True }

-- | Set the @Orderless@ attribute.
setOrderless :: Attributes -> Attributes
setOrderless attr = attr { orderless = True }

-- | Set the @NumericFunction@ attribute.
setNumericFunction :: Attributes -> Attributes
setNumericFunction attr = attr { numericFunction = True }

-- | Set the hold behavior of a symbol.
setHoldType :: HoldType -> Attributes -> Attributes
setHoldType ty attr = attr { holdType = Just ty }

-- | Information associated with a symbol in the evaluation context.
data SymbolRecord = MkSymbolRecord
  { ownValue   :: !(Maybe Expr)
  , downValues :: !DownValues -- ^ Rules that match expressions where
                              -- the given symbol is the head
  , upValues   :: !(Seq Rule) -- ^ A rule that matches expressions where
                              -- the given symbol is 1 level below the
                              -- head.
  , attributes :: !Attributes
  }
  deriving (Show)

emptySymbolRecord :: Symbol -> SymbolRecord
emptySymbolRecord _ = MkSymbolRecord Nothing emptyDownValues Seq.empty EmptyAttributes

modifyRecordOwnValue :: (Maybe Expr -> Maybe Expr) -> SymbolRecord -> SymbolRecord
modifyRecordOwnValue f record = record { ownValue = f record.ownValue }

modifyRecordDownValues :: (DownValues -> DownValues) -> SymbolRecord -> SymbolRecord
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
    isEmpty (MkSymbolRecord Nothing downVals Empty EmptyAttributes)
      | Seq.null downVals.sequentialRules
      , Map.null downVals.uniqueMatches
      = True
    isEmpty _ = False

-- | Look up the record associated with a symbol in the current context.
lookupSymbolRecord :: Symbol -> Eval (Maybe SymbolRecord)
lookupSymbolRecord sym = do
  ctx <- getContext
  liftIO $ HT.lookup ctx.symbolRecordTable sym

lookupSymbolRecordDefault :: Symbol -> Eval SymbolRecord
lookupSymbolRecordDefault sym =
  fmap (maybe (emptySymbolRecord sym) id) $ lookupSymbolRecord sym

-- | Look up the attributes of a symbol in the current context.
lookupAttributes :: Symbol -> Eval Attributes
lookupAttributes = fmap (.attributes) . lookupSymbolRecordDefault

modifyOwnValue :: Symbol -> (Maybe Expr -> Maybe Expr) -> Eval ()
modifyOwnValue sym f = modifyRecord sym (modifyRecordOwnValue f)

modifyDownValues :: Symbol -> (DownValues -> DownValues) -> Eval ()
modifyDownValues sym f = modifyRecord sym (modifyRecordDownValues f)

modifyUpValues :: Symbol -> (Seq Rule -> Seq Rule) -> Eval ()
modifyUpValues sym f = modifyRecord sym (modifyRecordUpValues f)

-- | Add a down-value rule for the given symbol.
addDownValue :: Symbol -> Rule -> Eval ()
addDownValue sym rule = modifyDownValues sym (addRule rule)
  where
    addRule :: Rule -> DownValues -> DownValues
    addRule newRule downVals = case newRule of
      PatRule pat rhs
        | Just expr <- matchesUniqueExpr pat ->
          downVals { uniqueMatches = Map.insert expr rhs downVals.uniqueMatches }
      _ -> downVals { sequentialRules = downVals.sequentialRules |> newRule }

-- | Add an up-value rule for the given symbol.
addUpValue :: Symbol -> Rule -> Eval ()
addUpValue sym rule = modifyUpValues sym (|> rule)

-- | A declaration that can be added to the evaluation context.
data Decl
  = -- | An own-value assignment for a symbol.
    OwnValue Symbol Expr
  | -- | A down-value rule for expressions with the given head.
    DownValue Symbol Rule
  | -- | An up-value rule for expressions containing the given symbol.
    UpValue Symbol Rule
  deriving (Show)

-- | Add a declaration to the current context.
addDecl :: Decl -> Eval ()
addDecl = \case
  OwnValue  sym expr -> modifyOwnValue sym (const (Just expr))
  DownValue sym rule -> addDownValue   sym rule
  UpValue   sym rule -> addUpValue     sym rule

-- | Modify the attributes of a symbol in the current context.
modifyAttributes :: Symbol -> (Attributes -> Attributes) -> Eval ()
modifyAttributes sym f = modifyRecord sym (modifyRecordAttributes f)

-- | Set the attributes of a symbol in the current context.
setAttributes :: Symbol -> Attributes -> Eval ()
setAttributes sym attrs = modifyAttributes sym (const attrs)

-- | Clear the values associated with a symbol, leaving its attributes.
clear :: Symbol -> Eval ()
clear sym = modifyRecord sym $
  modifyRecordOwnValue (const Nothing) .
  modifyRecordDownValues (const emptyDownValues) .
  modifyRecordUpValues (const Seq.empty)

-- | Clear the values and attributes associated with a symbol.
clearAll :: Symbol -> Eval ()
clearAll sym = modifyRecord sym (const (emptySymbolRecord sym))

-- | Create a fresh module-local symbol derived from the given base symbol.
newModuleSymbol :: Symbol -> Eval Symbol
newModuleSymbol x = do
  ctx <- ask
  n   <- liftIO $ atomicModifyIORef' ctx.moduleNumberRef $ \n -> (n+1,n)
  pure $
    symbolFromShortText $
    symbolToShortText x <> ShortText.pack ("$" <> show n)

-- | Get the symbols that currently have records in the context.
getDefinedSymbols :: Eval [Symbol]
getDefinedSymbols = do
  ctx <- ask
  liftIO $
    fmap (map fst) $ HT.toList ctx.symbolRecordTable

patAppTypeFromAttributes :: Symbol -> Attributes -> PatAppType
patAppTypeFromAttributes sym attr = case (attr.flat, attr.orderless) of
  (False, False) -> PatAppFree
  (True,  False) -> PatAppA sym
  (False, True ) -> PatAppC
  (True,  True ) -> PatAppAC sym

lookupPatAppType :: Symbol -> Eval PatAppType
lookupPatAppType sym = patAppTypeFromAttributes sym <$> lookupAttributes sym

-- | Compile an expression into a pattern, using the current context to
-- determine attributes such as associativity and commutativity.
compilePat :: Expr -> Eval Pat
compilePat = patFromExpr lookupPatAppType
