{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}

module MicroMath.Context
  ( Rule(..)
  , Context(..)
  , Attributes(..)
  , emptyAttributes
  , SymbolRecord(..)
  , ContextM
  , emptyContext
  , createContext
  , lookupSymbol
  , lookupAttributes
  , allRules
  , addDownValue
  , addUpValue
  , addPatRule
  , modifyAttributes
  , setAttributes
  , clear
  , clearAll
  ) where

import Control.Monad.State (State, execState, modify')
import Data.Foldable       qualified as Foldable
import Data.IntMap.Strict     (IntMap)
import Data.IntMap.Strict     qualified as IntMap
import Data.Sequence       (Seq, (|>))
import Data.Sequence       qualified as Seq
import MicroMath.Expr      (Expr (..), Literal (..))
import MicroMath.Pat       (Pat (..), rootSymbol)
import MicroMath.PPrint    (PPrint (..))
import MicroMath.Symbol    (Symbol, symbolIndex)

data Rule
  = PatRule Pat Expr
  | BuiltinRule (Expr -> Maybe Expr)

instance Show Rule where
  show (PatRule p expr) = pPrint p ++ " := " ++ pPrint expr
  show (BuiltinRule _)  = "<BuiltinRule>"

instance PPrint Rule where
  pPrint = show

-- | TODO: Add Protected, NumericFunction, OneIdentity
data Attributes = MkAttributes
  { flat      :: !Bool
  , orderless :: !Bool
  } deriving (Eq, Ord, Show)

pattern EmptyAttributes :: Attributes
pattern EmptyAttributes = MkAttributes False False

emptyAttributes :: Attributes
emptyAttributes = EmptyAttributes

data SymbolRecord = MkSymbolRecord
  { ownValue   :: !(Maybe Expr)
  , downValues :: !(Seq Rule) -- ^ A rule that matches expressions where
                              -- the given symbol is the head
  , upValues   :: !(Seq Rule) -- ^ A rule that matches expressions where
                              -- the given symbol is 1 level below the
                              -- head. (TODO: More cases?)
  , attributes :: !Attributes
  }
  deriving (Show)

emptySymbolRecord :: SymbolRecord
emptySymbolRecord = MkSymbolRecord Nothing Seq.empty Seq.empty EmptyAttributes

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

lookupSymbol :: Symbol -> Context -> Maybe SymbolRecord
lookupSymbol s (MkContext ctx) = IntMap.lookup (symbolIndex s) ctx

-- | Apply the given function to a record, and remove the record from
-- the map if it is empty.
modifyRecord' :: Symbol -> (SymbolRecord -> SymbolRecord) -> Context -> Context
modifyRecord' sym f (MkContext m) = MkContext $
  IntMap.alter (checkNotEmpty . changeRecord) (symbolIndex sym) m
  where
    changeRecord = f . maybe emptySymbolRecord id
    checkNotEmpty (MkSymbolRecord Nothing Seq.Empty Seq.Empty EmptyAttributes) = Nothing
    checkNotEmpty r = Just r

-- | A monad for stringing together modifications to Context
type ContextM = State Context

createContext :: ContextM () -> Context
createContext cm = execState cm emptyContext

modifyRecord :: Symbol -> (SymbolRecord -> SymbolRecord) -> ContextM ()
modifyRecord sym f = modify' (modifyRecord' sym f)

lookupSymbolDefault :: Symbol -> Context -> SymbolRecord
lookupSymbolDefault sym = maybe emptySymbolRecord id . lookupSymbol sym

lookupAttributes :: Symbol -> Context -> Attributes
lookupAttributes sym ctx = (lookupSymbolDefault sym ctx).attributes

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

addPatRule :: Pat -> Expr -> ContextM ()
addPatRule pat expr
  | PatAtom _ (LitSymbol sym) <- pat =
      modifyOwnValue sym (const (Just expr))
  | Just sym <- rootSymbol pat = addDownValue sym (PatRule pat expr)
  | otherwise = error "Pattern has no root symbol"

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
