{-# LANGUAGE NoFieldSelectors #-}

module Howl.Eval.EvalCache
  ( EvalCache
  , newEvalCache
  , lookupEvalCache
  , insertEvalCache
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar)
import Control.Exception       (evaluate)
import Data.Hashable           (Hashable (..))
import Data.HashTable.IO       qualified as HT
import GHC.StableName          (StableName, hashStableName, makeStableName)
import Howl.Expr.Internal (Expr)
import System.Mem.Weak         (Weak, mkWeakPtr)

newtype SN a = SN (StableName a)

instance Eq (SN a) where
  SN a == SN b = a == b

instance Hashable (SN a) where
  hashWithSalt salt (SN sn) = hashWithSalt salt (hashStableName sn)

type Table = HT.BasicHashTable (SN Expr) (Weak Expr)

data EvalCache = EvalCache
  { _ecLock  :: !(MVar ())
  , _ecTable :: !Table
  }

newEvalCache :: IO EvalCache
newEvalCache = EvalCache <$> newMVar () <*> HT.new

lookupEvalCache :: EvalCache -> Expr -> IO Bool
lookupEvalCache (EvalCache lock table) expr0 = do
  !expr <- evaluate expr0
  sn <- SN <$> makeStableName expr
  modifyMVar lock $ \() -> do
    mb <- HT.lookup table sn
    case mb of
      Nothing -> pure ((), False)
      Just _  -> pure ((), True)

insertEvalCache :: EvalCache -> Expr -> IO ()
insertEvalCache (EvalCache lock table) expr0 = do
  !expr <- evaluate expr0
  sn <- SN <$> makeStableName expr
  -- Important: finalizer must not capture 'expr'
  let finalizer = modifyMVar_ lock $ \() -> HT.delete table sn
  w <- mkWeakPtr expr (Just finalizer)
  modifyMVar_ lock $ \() -> HT.insert table sn w
