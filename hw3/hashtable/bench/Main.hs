module Main where

import Control.Concurrent.Async (mapConcurrently_)
import Control.DeepSeq (NFData (..), deepseq)
import Control.Monad (replicateM, replicateM_)
import Criterion.Main
import Data.Hashable (Hashable)
import System.Random (getStdRandom, random, randomR)

import Hashtable (ConcurrentHashTable (..), getCHT, newCHT, putCHT, sizeCHT)

-- | Generate random sample Char key.
rndKey :: IO Char
rndKey = getStdRandom (randomR ('a', 'z'))

-- | Generate random sample (Char, Int) pair.
rndPair :: IO (Char, Int)
rndPair = do
  key <- rndKey
  val <- getStdRandom random
  return (key, val)

-- | Generate sample hashtable with random initial contents.
sampleCHT :: IO (ConcurrentHashTable Char Int)
sampleCHT = do
  cht     <- newCHT
  pairCnt <- getStdRandom (randomR (1 :: Int, 100))
  replicateM_ pairCnt rndPair
  return cht

-- | Type alias for a test operation to perform.
type TestOp = ConcurrentHashTable Char Int -> IO ()

-- | Generate a sample "get" operation.
genGetOp :: IO TestOp
genGetOp = do
  key <- rndKey
  return $ getFn key
 where
  getFn :: (Hashable k, Eq k, NFData v) => k -> ConcurrentHashTable k v -> IO ()
  getFn k cht = getCHT k cht >>= \v -> v `deepseq` return ()

-- | Generate a sample "size" operation.
genSizeOp :: IO TestOp
genSizeOp = return sizeFn
 where
  sizeFn :: ConcurrentHashTable k v -> IO ()
  sizeFn cht = sizeCHT cht >>= \v -> v `deepseq` return ()

-- | Generate a sample "put" operation.
genPutOp :: IO TestOp
genPutOp = do
  (key, val) <- rndPair
  return $ putCHT key val

-- | Generate a sample random hashtable operation; probability: 80% put, 10% get, 10% size.
genRandomOp :: IO TestOp
genRandomOp = do
  opType <- getStdRandom (randomR (1 :: Int, 10))
  case opType of
    1 -> genGetOp
    2 -> genSizeOp
    _ -> genPutOp

-- | Test set of 10^5 sample "put" operations.
putOpsEnv :: IO [TestOp]
putOpsEnv = replicateM (10 ^ (5 :: Int)) genPutOp

-- | Test set of 10^5 sample "get" operations.
getOpsEnv :: IO [TestOp]
getOpsEnv = replicateM (10 ^ (5 :: Int)) genGetOp

-- | Test set of 10^5 sample "size" operations.
sizeOpsEnv :: IO [TestOp]
sizeOpsEnv = replicateM (10 ^ (5 :: Int)) genSizeOp

-- | Test set of 10^5 sample random operations.
allOpsEnv :: IO [TestOp]
allOpsEnv = replicateM (10 ^ (5 :: Int)) genRandomOp

-- | Given a sample list of test operations, perform them concurrently to test the hashtable.
testOps
  :: IO (ConcurrentHashTable Char Int)
  -> [ConcurrentHashTable Char Int -> IO ()]
  -> IO (ConcurrentHashTable Char Int)
testOps chtF ops = do
  cht <- chtF
  mapConcurrently_ (\f -> f cht) ops
  return cht

-- | Execute "put", "get", "size" and all operation benchmarks.
main :: IO ()
main = defaultMain
  [ env putOpsEnv $ \ops -> bench "Put operations" $ whnfIO $ testOps newCHT ops
  , env getOpsEnv
    $ \ops -> bench "Get operations" $ whnfIO $ testOps sampleCHT ops
  , env sizeOpsEnv
    $ \ops -> bench "Size operations" $ whnfIO $ testOps sampleCHT ops
  , env allOpsEnv $ \ops -> bench "All operations" $ whnfIO $ testOps newCHT ops
  ]
