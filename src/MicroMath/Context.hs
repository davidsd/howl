{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}

module MicroMath.Context
  ( Rule(..)
  , functionRule
  , Context(..)
  , Attributes(..)
  , HoldType(..)
  , emptyAttributes
  , SymbolRecord(..)
  , ContextM
  , Decl(..)
  , emptyContext
  , createContext
  , lookupSymbolRecord
  , lookupAttributes
  , allRules
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
  ) where

import Control.Monad.State (State, execState, modify')
import Data.Foldable       qualified as Foldable
import Data.IntMap.Strict  (IntMap)
import Data.IntMap.Strict  qualified as IntMap
import Data.Sequence       (Seq, (|>))
import Data.Sequence       qualified as Seq
import MicroMath.Expr      (Expr (..))
import MicroMath.Pat       (Pat (..))
import MicroMath.PPrint    (PPrint (..))
import MicroMath.Symbol    (Symbol, symbolIndex)

data Rule
  = PatRule Pat Expr
  | BuiltinRule (Context -> Expr -> Maybe Expr)

instance Show Rule where
  show (PatRule p expr) = pPrint p ++ " := " ++ pPrint expr
  show (BuiltinRule _)  = "<BuiltinRule>"

functionRule :: (Expr -> Maybe Expr) -> Rule
functionRule f = BuiltinRule (const f)

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

newtype Context = MkContext (IntMap SymbolRecord)
  deriving (Show)

emptyContext :: Context
emptyContext = MkContext IntMap.empty

lookupSymbolRecord :: Symbol -> Context -> Maybe SymbolRecord
lookupSymbolRecord s (MkContext ctx) = IntMap.lookup (symbolIndex s) ctx

-- | Apply the given function to a record, and remove the record from
-- the map if it is empty.
modifyRecord' :: Symbol -> (SymbolRecord -> SymbolRecord) -> Context -> Context
modifyRecord' sym f (MkContext m) = MkContext $
  IntMap.alter (checkNotEmpty . changeRecord) (symbolIndex sym) m
  where
    changeRecord = f . maybe (emptySymbolRecord sym) id
    checkNotEmpty (MkSymbolRecord _ Nothing Seq.Empty Seq.Empty EmptyAttributes) = Nothing
    checkNotEmpty r = Just r

-- | A monad for stringing together modifications to Context
type ContextM = State Context

createContext :: ContextM () -> Context
createContext cm = execState cm emptyContext

modifyRecord :: Symbol -> (SymbolRecord -> SymbolRecord) -> ContextM ()
modifyRecord sym f = modify' (modifyRecord' sym f)

lookupSymbolRecordDefault :: Symbol -> Context -> SymbolRecord
lookupSymbolRecordDefault sym = maybe (emptySymbolRecord sym) id . lookupSymbolRecord sym

lookupAttributes :: Symbol -> Context -> Attributes
lookupAttributes sym ctx = (lookupSymbolRecordDefault sym ctx).attributes

modifyOwnValue :: Symbol -> (Maybe Expr -> Maybe Expr) -> ContextM ()
modifyOwnValue sym f = modifyRecord sym (modifyRecordOwnValue f)

modifyDownValues :: Symbol -> (Seq Rule -> Seq Rule) -> ContextM ()
modifyDownValues sym f = modifyRecord sym (modifyRecordDownValues f)

modifyUpValues :: Symbol -> (Seq Rule -> Seq Rule) -> ContextM ()
modifyUpValues sym f = modifyRecord sym (modifyRecordUpValues f)

addDownValue :: Symbol -> Rule -> ContextM ()
addDownValue sym rule = modifyDownValues sym (|> rule)

addUpValue :: Symbol -> Rule -> ContextM ()
addUpValue sym rule = modifyUpValues sym (|> rule)

data Decl
  = OwnValue Symbol Expr
  | DownValue Symbol Rule
  | UpValue Symbol Rule

addDecl :: Decl -> ContextM ()
addDecl = \case
  OwnValue sym expr  -> modifyOwnValue sym (const (Just expr))
  DownValue sym rule -> addDownValue sym rule
  UpValue sym rule   -> addUpValue sym rule

modifyAttributes :: Symbol -> (Attributes -> Attributes) -> ContextM ()
modifyAttributes sym f = modifyRecord sym (modifyRecordAttributes f)

setAttributes :: Symbol -> Attributes -> ContextM ()
setAttributes sym attrs = modifyAttributes sym (const attrs)

clear :: Symbol -> ContextM ()
clear sym = modifyRecord sym $
  modifyRecordOwnValue (const Nothing) .
  modifyRecordDownValues (const Seq.empty) .
  modifyRecordUpValues (const Seq.empty)

clearAll :: Symbol -> ContextM ()
clearAll sym = modify' $ \(MkContext m) ->
  MkContext (IntMap.delete (symbolIndex sym) m)

allRules :: Context -> [Rule]
allRules (MkContext ctx) = do
  record <- IntMap.elems ctx
  Foldable.toList (record.downValues <> record.upValues)
