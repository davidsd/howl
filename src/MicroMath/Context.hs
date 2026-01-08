{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}

module MicroMath.Context
  ( Rule(..)
  , Context(..)
  , Attribute(..)
  , SymbolRecord(..)
  , emptyContext
  , allRules
  , lookupAttributes
  , lookupSymbol
  ) where

import Control.Applicative (Alternative, empty)
import Control.Monad       (foldM, guard)
import Data.List           (sort)
import Data.Map.Strict     (Map)
import Data.Map.Strict     qualified as Map
import Data.Set            (Set)
import Data.Set            qualified as Set
import MicroMath.Expr      (Expr (..), Literal (..), Symbol,
                            flattenSequences, flattenWithHead, mapSymbols)
import MicroMath.Pat       (Pat (..), SeqType (..), addNames, rootSymbol)
import MicroMath.Util      (splits, splits1, subSequences)

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

newtype Context = MkContext (Map Symbol SymbolRecord)

emptyContext :: Context
emptyContext = MkContext Map.empty

lookupSymbol :: Symbol -> Context -> Maybe SymbolRecord
lookupSymbol s (MkContext ctx) = Map.lookup s ctx

-- | Apply the given function to a record, and remove the record from
-- the map if it is empty.
modifyRecord :: Symbol -> (SymbolRecord -> SymbolRecord) -> Context -> Context
modifyRecord sym f (MkContext m) = MkContext $
  Map.alter (checkNotEmpty . changeRecord) sym m
  where
    changeRecord = f . maybe emptySymbolRecord id
    checkNotEmpty (MkSymbolRecord Nothing [] [] s)
      | Set.null s = Nothing
    checkNotEmpty r = Just r

lookupSymbolDefault :: Symbol -> Context -> SymbolRecord
lookupSymbolDefault sym = maybe emptySymbolRecord id . lookupSymbol sym

lookupAttributes :: Symbol -> Context -> Set Attribute
lookupAttributes sym ctx = (lookupSymbolDefault sym ctx).attributes

modifyRecordOwnValue :: (Maybe Expr -> Maybe Expr) -> SymbolRecord -> SymbolRecord
modifyRecordOwnValue f record = record { ownValue = f record.ownValue }

modifyRecordDownValues :: ([Rule] -> [Rule]) -> SymbolRecord -> SymbolRecord
modifyRecordDownValues f record = record { downValues = f record.downValues }

modifyRecordUpValues :: ([Rule] -> [Rule]) -> SymbolRecord -> SymbolRecord
modifyRecordUpValues f record = record { upValues = f record.upValues }

modifyRecordAttributes :: (Set Attribute -> Set Attribute) -> SymbolRecord -> SymbolRecord
modifyRecordAttributes f record = record { attributes = f record.attributes }

modifyDownValues :: Symbol -> ([Rule] -> [Rule]) -> Context -> Context
modifyDownValues sym f = modifyRecord sym (modifyRecordDownValues f)

modifyUpValues :: Symbol -> ([Rule] -> [Rule]) -> Context -> Context
modifyUpValues sym f = modifyRecord sym (modifyRecordUpValues f)

addDownValue :: Symbol -> Rule -> Context -> Context
addDownValue sym rule = modifyDownValues sym (rule :)

addUpValue :: Symbol -> Rule -> Context -> Context
addUpValue sym rule = modifyUpValues sym (rule :)

addPatRule :: Pat -> Expr -> Context -> Context
addPatRule pat expr = case rootSymbol pat of
  Just sym -> addDownValue sym (PatRule pat expr)
  Nothing  -> error "Root symbol of the given pattern is not a symbol"

clear :: Symbol -> Context -> Context
clear sym = modifyRecord sym $
  modifyRecordOwnValue (const Nothing) .
  modifyRecordDownValues (const []) .
  modifyRecordUpValues (const [])

clearAll :: Symbol -> Context -> Context
clearAll sym (MkContext m) = MkContext (Map.delete sym m)

allRules :: Context -> [Rule]
allRules (MkContext ctx) = do
  record <- Map.elems ctx
  record.downValues <> record.upValues
