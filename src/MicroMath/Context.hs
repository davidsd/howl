{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}

module MicroMath.Context
  ( Rule(..)
  , Context(..)
  , Attribute(..)
  , SymbolRecord(..)
  , ContextM
  , emptyContext
  , createContext
  , allRules
  , lookupAttributes
  , lookupSymbol
  , setAttributes
  , addDownValue
  , addUpValue
  , addPatRule
  , clear
  , clearAll
  ) where

import Control.Monad.State (State, execState, modify')
import Data.Map.Strict     (Map)
import Data.Map.Strict     qualified as Map
import Data.Set            (Set)
import Data.Set            qualified as Set
import MicroMath.Expr      (Expr (..), Literal (..), Symbol)
import MicroMath.Pat       (Pat (..), rootSymbol)

data Rule
  = PatRule Pat Expr
  | BuiltinRule (Expr -> Maybe Expr)

-- | TODO: Add Protected, NumericFunction, OneIdentity
data Attribute
  = Flat
  | Orderless
  deriving (Eq, Ord, Show)

data SymbolRecord = MkSymbolRecord
  { ownValue   :: Maybe Expr
  , downValues :: [Rule] -- ^ A rule that matches expressions where
                         -- the given symbol is the head
  , upValues   :: [Rule] -- ^ A rule that matches expressions where
                         -- the given symbol is 1 level below the
                         -- head. (TODO: More cases?)
  , attributes :: Set Attribute
  }

emptySymbolRecord :: SymbolRecord
emptySymbolRecord = MkSymbolRecord Nothing [] [] Set.empty

modifyRecordOwnValue :: (Maybe Expr -> Maybe Expr) -> SymbolRecord -> SymbolRecord
modifyRecordOwnValue f record = record { ownValue = f record.ownValue }

modifyRecordDownValues :: ([Rule] -> [Rule]) -> SymbolRecord -> SymbolRecord
modifyRecordDownValues f record = record { downValues = f record.downValues }

modifyRecordUpValues :: ([Rule] -> [Rule]) -> SymbolRecord -> SymbolRecord
modifyRecordUpValues f record = record { upValues = f record.upValues }

modifyRecordAttributes :: (Set Attribute -> Set Attribute) -> SymbolRecord -> SymbolRecord
modifyRecordAttributes f record = record { attributes = f record.attributes }

newtype Context = MkContext (Map Symbol SymbolRecord)

emptyContext :: Context
emptyContext = MkContext Map.empty

lookupSymbol :: Symbol -> Context -> Maybe SymbolRecord
lookupSymbol s (MkContext ctx) = Map.lookup s ctx

-- | Apply the given function to a record, and remove the record from
-- the map if it is empty.
modifyRecord' :: Symbol -> (SymbolRecord -> SymbolRecord) -> Context -> Context
modifyRecord' sym f (MkContext m) = MkContext $
  Map.alter (checkNotEmpty . changeRecord) sym m
  where
    changeRecord = f . maybe emptySymbolRecord id
    checkNotEmpty (MkSymbolRecord Nothing [] [] s)
      | Set.null s = Nothing
    checkNotEmpty r = Just r

-- | A monad for stringing together modifications to Context
type ContextM = State Context

createContext :: ContextM () -> Context
createContext cm = execState cm emptyContext

modifyRecord :: Symbol -> (SymbolRecord -> SymbolRecord) -> ContextM ()
modifyRecord sym f = modify' (modifyRecord' sym f)

lookupSymbolDefault :: Symbol -> Context -> SymbolRecord
lookupSymbolDefault sym = maybe emptySymbolRecord id . lookupSymbol sym

lookupAttributes :: Symbol -> Context -> Set Attribute
lookupAttributes sym ctx = (lookupSymbolDefault sym ctx).attributes

modifyOwnValue :: Symbol -> (Maybe Expr -> Maybe Expr) -> ContextM ()
modifyOwnValue sym f = modifyRecord sym (modifyRecordOwnValue f)

modifyDownValues :: Symbol -> ([Rule] -> [Rule]) -> ContextM ()
modifyDownValues sym f = modifyRecord sym (modifyRecordDownValues f)

modifyUpValues :: Symbol -> ([Rule] -> [Rule]) -> ContextM ()
modifyUpValues sym f = modifyRecord sym (modifyRecordUpValues f)

modifyAttributes :: Symbol -> (Set Attribute -> Set Attribute) -> ContextM ()
modifyAttributes sym f = modifyRecord sym (modifyRecordAttributes f)

addDownValue :: Symbol -> Rule -> ContextM ()
addDownValue sym rule = modifyDownValues sym (++ [rule])

addUpValue :: Symbol -> Rule -> ContextM ()
addUpValue sym rule = modifyUpValues sym (++ [rule])

addPatRule :: Pat -> Expr -> ContextM ()
addPatRule pat expr
  | PatAtom _ (LitSymbol sym) <- pat =
      modifyOwnValue sym (const (Just expr))
  | Just sym <- rootSymbol pat = addDownValue sym (PatRule pat expr)
  | otherwise = error "Pattern has no root symbol"

setAttributes :: Symbol -> [Attribute] -> ContextM ()
setAttributes sym attrs = modifyAttributes sym (const (Set.fromList attrs))

clear :: Symbol -> ContextM ()
clear sym = modifyRecord sym $
  modifyRecordOwnValue (const Nothing) .
  modifyRecordDownValues (const []) .
  modifyRecordUpValues (const [])

clearAll :: Symbol -> ContextM ()
clearAll sym = modify' $ \(MkContext m) ->
  MkContext (Map.delete sym m)

allRules :: Context -> [Rule]
allRules (MkContext ctx) = do
  record <- Map.elems ctx
  record.downValues <> record.upValues
