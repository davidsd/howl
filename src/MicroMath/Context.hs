{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}

module MicroMath.Context
  ( Rule(..)
  , Context(..)
  , Attributes(..)
  , HoldType(..)
  , emptyAttributes
  , SymbolRecord(..)
  , EvalM(..)
  , runEvalM
  , getContext
  , Decl(..)
  , emptyContext
  , createContext
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
  ) where

import Control.Monad.State.Strict (MonadState, State, evalState, execState, get,
                                   modify', state)
import Data.IntMap.Strict         (IntMap)
import Data.IntMap.Strict         qualified as IntMap
import Data.Sequence              (Seq, (|>))
import Data.Sequence              qualified as Seq
import Data.Text.Short            qualified as ShortText
import MicroMath.Expr             (Expr (..))
import MicroMath.Pat              (Pat (..))
import MicroMath.PPrint           (PPrint (..))
import MicroMath.Symbol           (Symbol, symbolFromShortText, symbolIndex,
                                   symbolToShortText)

data Context = MkContext
  { symbolMap    :: !(IntMap SymbolRecord)
  , moduleNumber :: !Int
  } deriving (Show)

emptyContext :: Context
emptyContext = MkContext IntMap.empty 0

newtype EvalM a = EvalM (State Context a)
  deriving newtype (Functor, Applicative, Monad, MonadState Context)

runEvalM :: Context -> EvalM a -> a
runEvalM ctx (EvalM f) = evalState f ctx

getContext :: EvalM Context
getContext = get

data Rule
  = PatRule Pat Expr
  | BuiltinRule (Expr -> EvalM (Maybe Expr))

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
  { symbol     :: !Symbol
  , ownValue   :: !(Maybe Expr)
  , downValues :: !(Seq Rule) -- ^ A rule that matches expressions where
                              -- the given symbol is the head
  , upValues   :: !(Seq Rule) -- ^ A rule that matches expressions where
                              -- the given symbol is 1 level below the
                              -- head.
  , attributes :: !Attributes
  }
  deriving (Show)

emptySymbolRecord :: Symbol -> SymbolRecord
emptySymbolRecord sym = MkSymbolRecord sym Nothing Seq.empty Seq.empty EmptyAttributes

modifyRecordOwnValue :: (Maybe Expr -> Maybe Expr) -> SymbolRecord -> SymbolRecord
modifyRecordOwnValue f record = record { ownValue = f record.ownValue }

modifyRecordDownValues :: (Seq Rule -> Seq Rule) -> SymbolRecord -> SymbolRecord
modifyRecordDownValues f record = record { downValues = f record.downValues }

modifyRecordUpValues :: (Seq Rule -> Seq Rule) -> SymbolRecord -> SymbolRecord
modifyRecordUpValues f record = record { upValues = f record.upValues }

modifyRecordAttributes :: (Attributes -> Attributes) -> SymbolRecord -> SymbolRecord
modifyRecordAttributes f record = record { attributes = f record.attributes }

lookupSymbolRecord :: Symbol -> Context -> Maybe SymbolRecord
lookupSymbolRecord s ctx = IntMap.lookup (symbolIndex s) ctx.symbolMap

-- | Apply the given function to a record, and remove the record from
-- the map if it is empty.
modifyRecord' :: Symbol -> (SymbolRecord -> SymbolRecord) -> Context -> Context
modifyRecord' sym f ctx = ctx { symbolMap = newSymbolMap }
  where
    changeRecord = f . maybe (emptySymbolRecord sym) id

    checkNotEmpty (MkSymbolRecord _ Nothing Seq.Empty Seq.Empty EmptyAttributes) = Nothing
    checkNotEmpty r = Just r

    newSymbolMap = IntMap.alter (checkNotEmpty . changeRecord) (symbolIndex sym) ctx.symbolMap

createContext :: State Context () -> Context
createContext cm = execState cm emptyContext

modifyRecord :: MonadState Context m => Symbol -> (SymbolRecord -> SymbolRecord) -> m ()
modifyRecord sym f = modify' (modifyRecord' sym f)

lookupSymbolRecordDefault :: Symbol -> Context -> SymbolRecord
lookupSymbolRecordDefault sym = maybe (emptySymbolRecord sym) id . lookupSymbolRecord sym

lookupAttributes :: Symbol -> Context -> Attributes
lookupAttributes sym ctx = (lookupSymbolRecordDefault sym ctx).attributes

modifyOwnValue :: MonadState Context m => Symbol -> (Maybe Expr -> Maybe Expr) -> m ()
modifyOwnValue sym f = modifyRecord sym (modifyRecordOwnValue f)

modifyDownValues :: MonadState Context m => Symbol -> (Seq Rule -> Seq Rule) -> m ()
modifyDownValues sym f = modifyRecord sym (modifyRecordDownValues f)

modifyUpValues :: MonadState Context m => Symbol -> (Seq Rule -> Seq Rule) -> m ()
modifyUpValues sym f = modifyRecord sym (modifyRecordUpValues f)

addDownValue :: MonadState Context m => Symbol -> Rule -> m ()
addDownValue sym rule = modifyDownValues sym (|> rule)

addUpValue :: MonadState Context m => Symbol -> Rule -> m ()
addUpValue sym rule = modifyUpValues sym (|> rule)

data Decl
  = OwnValue Symbol Expr
  | DownValue Symbol Rule
  | UpValue Symbol Rule

addDecl :: MonadState Context m => Decl -> m ()
addDecl = \case
  OwnValue sym expr  -> modifyOwnValue sym (const (Just expr))
  DownValue sym rule -> addDownValue sym rule
  UpValue sym rule   -> addUpValue sym rule

modifyAttributes :: MonadState Context m => Symbol -> (Attributes -> Attributes) -> m ()
modifyAttributes sym f = modifyRecord sym (modifyRecordAttributes f)

setAttributes :: MonadState Context m => Symbol -> Attributes -> m ()
setAttributes sym attrs = modifyAttributes sym (const attrs)

clear :: MonadState Context m => Symbol -> m ()
clear sym = modifyRecord sym $
  modifyRecordOwnValue (const Nothing) .
  modifyRecordDownValues (const Seq.empty) .
  modifyRecordUpValues (const Seq.empty)

clearAll :: MonadState Context m => Symbol -> m ()
clearAll sym = modify' $ \ctx ->
  ctx { symbolMap = IntMap.delete (symbolIndex sym) ctx.symbolMap }

newModuleSymbol :: MonadState Context m => Symbol -> m Symbol
newModuleSymbol x = state $ \ctx ->
  let
    n = ctx.moduleNumber
    xNew =
      symbolFromShortText $
      symbolToShortText x <> ShortText.pack ("$" <> show n)
  in
    (xNew, ctx { moduleNumber = n + 1})
