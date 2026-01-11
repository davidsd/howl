{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

module MicroMath.Symbol.Table
  ( Unique(..)
  , Symbol(..)
  , mkSymbol
  , symbolName
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.DeepSeq         (NFData)
import Data.Bits               ((.&.))
import Data.Hashable           (Hashable (..))
import Data.HashTable.IO       qualified as HT
import Data.IORef              (IORef, atomicModifyIORef', newIORef)
import Data.Primitive.Array    (Array, unsafeFreezeArray, newArray, indexArray, writeArray)
import Data.String             (IsString (..))
import Data.Text               (Text)
import Data.Text               qualified as Text
import Data.Text.Short         (ShortText)
import Data.Text.Short         qualified as TS
import MicroMath.PPrint        (PPrint (..))
import System.IO.Unsafe        (unsafePerformIO)

-- ---------- Public types ----------

newtype Unique = Unique { getUnique :: Int }
  deriving newtype (Eq, Ord, Show, NFData, Hashable)

-- Store ONLY the identity. This is what you want in your AST.
newtype Symbol = Symbol { symUnique :: Unique }
  deriving newtype (Eq, Ord, NFData, Hashable)

instance Show Symbol where
  show = Text.unpack . symbolName

-- ---------- Internals ----------

type InternTable = HT.BasicHashTable ShortText Unique
type ReverseTable = HT.BasicHashTable Unique ShortText

{-# NOINLINE internTable #-}
internTable :: InternTable
internTable = unsafePerformIO HT.new

{-# NOINLINE reverseTable #-}
reverseTable :: ReverseTable
reverseTable = unsafePerformIO HT.new
-- If you don't want reverse lookup, delete this and the inserts.

{-# NOINLINE nextUnique #-}
nextUnique :: IORef Int
nextUnique = unsafePerformIO (newIORef 0)

numStripes :: Int
numStripes = 256  -- must be a power of 2 for the mask trick below

{-# NOINLINE stripes #-}
stripes :: Array (MVar ())
stripes = unsafePerformIO $ do
  -- dummy value; we overwrite every element before freezing
  dummy <- newMVar ()
  marr  <- newArray numStripes dummy
  let go !i
        | i == numStripes = pure ()
        | otherwise = do
            mv <- newMVar ()
            writeArray marr i mv
            go (i + 1)
  go 0
  unsafeFreezeArray marr

stripeForHash :: Int -> Int
stripeForHash h = h .&. (numStripes - 1)

withStripeHash :: Int -> IO a -> IO a
withStripeHash h act =
  let mv = indexArray stripes (stripeForHash h)
  in withMVar mv (const act)


freshUnique :: IO Unique
freshUnique =
  -- unique generation itself can be contention-light
  atomicModifyIORef' nextUnique $ \n -> (n + 1, Unique n)

-- ---------- The "pure" API ----------

{-# NOINLINE mkSymbol #-}
mkSymbol :: Text -> Symbol
mkSymbol !t = unsafePerformIO $ do
  let
    !k = TS.fromText t  -- compact key; stored once in table
    !h = hash k
  withStripeHash h $ do
    m <- HT.lookup internTable k
    case m of
      Just u  -> pure (Symbol u)
      Nothing -> do
        !u <- freshUnique
        HT.insert internTable k u
        HT.insert reverseTable u k  -- optional
        pure (Symbol u)

-- Optional reverse lookup (useful for debugging / printing)
symbolName :: Symbol -> Text
symbolName (Symbol u) =
  unsafePerformIO $ do
  m <- HT.lookup reverseTable u
  pure (maybe (error "Couldn't find symbol Text") TS.toText m)

instance PPrint Symbol where
  pPrint = show

instance IsString Symbol where
  fromString = mkSymbol . Text.pack

