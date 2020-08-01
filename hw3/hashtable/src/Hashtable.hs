{-# LANGUAGE ScopedTypeVariables #-}

module Hashtable
  ( ConcurrentHashTable(..)
  , newCHT
  , getCHT
  , putCHT
  , sizeCHT
  )
where

import Control.Concurrent.STM
import Control.Monad (when)
import Data.Hashable (Hashable, hash)
import Data.List (find)
import Data.Vector (Vector)
import qualified Data.Vector as V

-- | Type alias for hashtable buckets.
type Buckets k v = Vector (TVar [(k, v)])

-- | Hashtable record.
data ConcurrentHashTable k v = ConcurrentHashTable {
  chtBuckets  :: TVar (Buckets k v),
  chtSize     :: TVar Int,
  chtCapacity :: TVar Int
}

-- | Calculate bucket index by key and hashtable capacity.
indexByKey :: Hashable k => k -> Int -> Int
indexByKey key capacity = (hash key) `mod` capacity

-- | Appends key-value pair to list if absent, or replaces existing pair with that key.
insertKeyIntoList :: Eq k => k -> v -> [(k, v)] -> (Bool, [(k, v)])
insertKeyIntoList key value [] = (True, [(key, value)])
insertKeyIntoList key value ((k, v) : xs)
  | k == key  = (False, (k, value) : xs)
  | otherwise = (appended, (k, v) : next)
  where (appended, next) = insertKeyIntoList key value xs

-- | Instantiates a new empty concurrent hashtable.
newCHT :: IO (ConcurrentHashTable k v)
newCHT = atomically $ do
  let initialCapacity = 10
  buckets    <- V.replicateM initialCapacity (newTVar [])
  tvBuckets  <- newTVar buckets
  tvSize     <- newTVar 0
  tvCapacity <- newTVar initialCapacity
  return $ ConcurrentHashTable tvBuckets tvSize tvCapacity

-- | Retrieves a value from concurrent hashtable by key, or returns Nothing if absent.
getCHT :: (Hashable k, Eq k) => k -> ConcurrentHashTable k v -> IO (Maybe v)
getCHT key cht = atomically $ do
  capacity <- readTVar $ chtCapacity cht
  let index = indexByKey key capacity
  buckets       <- readTVar $ chtBuckets cht
  valuesAtIndex <- readTVar $ V.unsafeIndex buckets index
  let valuesMatchingKey = find ((== key) . fst) valuesAtIndex
  case valuesMatchingKey of
    Just (_, v) -> return $ Just v
    Nothing     -> return Nothing

-- | Relocates bucket's elements to a new hashtable storage.
arrangeBucket
  :: forall k v
   . (Hashable k, Eq k)
  => Int
  -> Vector (TVar [(k, v)])
  -> TVar [(k, v)]
  -> STM ()
arrangeBucket capacity newVector bucket = do
  elems <- readTVar bucket
  mapM_ arrangeElem elems
 where
  arrangeElem :: (k, v) -> STM ()
  arrangeElem (k, v) = do
    let index    = indexByKey k capacity
    let tvBucket = V.unsafeIndex newVector index
    b <- readTVar tvBucket
    let (_, newB) = insertKeyIntoList k v b
    writeTVar tvBucket newB

-- | Put key-value pair into the hashtable, or replace value for key if exists.
putCHT :: (Hashable k, Eq k) => k -> v -> ConcurrentHashTable k v -> IO ()
putCHT key value cht = atomically $ do
  let tvBuckets  = chtBuckets cht
  let tvSize     = chtSize cht
  let tvCapacity = chtCapacity cht
  size     <- readTVar tvSize
  capacity <- readTVar tvCapacity

  let needResizing = size == capacity
  when needResizing $ do
    let newCapacity = capacity * 2
    newBuckets <- V.replicateM newCapacity (newTVar [])
    buckets    <- readTVar tvBuckets
    V.mapM_ (arrangeBucket newCapacity newBuckets) buckets
    writeTVar tvBuckets  newBuckets
    writeTVar tvCapacity newCapacity

  let newCapacity = if needResizing then capacity * 2 else capacity
  let index       = indexByKey key newCapacity
  newBuckets <- readTVar tvBuckets
  let tvValuesAtIndex = V.unsafeIndex newBuckets index
  valuesAtIndex <- readTVar tvValuesAtIndex
  let (appended, newValuesAtIndex) = insertKeyIntoList key value valuesAtIndex
  writeTVar tvValuesAtIndex newValuesAtIndex

  when appended $ do
    writeTVar tvSize (size + 1)

-- | Query size of a hashtable.
sizeCHT :: ConcurrentHashTable k v -> IO Int
sizeCHT cht = readTVarIO $ chtSize cht
