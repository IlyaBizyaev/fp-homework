module HashtableSpec where

import Control.Monad (replicateM_)
import System.Random (getStdRandom, random, randomR)
import Test.Hspec

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

-- | Perform hashtable correctness tests.
spec :: Spec
spec = do
  describe "Hashtable" $ do
    it "Empty ht is 0-sized" $ do
      cht <- newCHT
      sz  <- sizeCHT cht
      sz `shouldBe` 0
    it "Insertion of new key increases ht size by 1" $ do
      cht <- sampleCHT
      sz0 <- sizeCHT cht
      putCHT '.' 42 cht
      sz1 <- sizeCHT cht
      sz0 + 1 `shouldBe` sz1
    it "Insertion of existing key does not change size" $ do
      cht <- sampleCHT
      putCHT '.' 42 cht
      sz0 <- sizeCHT cht
      putCHT '.' 11 cht
      sz1 <- sizeCHT cht
      sz0 `shouldBe` sz1
    it "Element is retrievable after insertion" $ do
      cht <- sampleCHT
      putCHT '.' 42 cht
      val <- getCHT '.' cht
      val `shouldBe` Just 42
    it "Insertion of pair with known key updates value" $ do
      cht <- sampleCHT
      putCHT '.' 42 cht
      putCHT '.' 11 cht
      val <- getCHT '.' cht
      val `shouldBe` Just 11
    it "Retrieval does not delete element" $ do
      cht <- sampleCHT
      putCHT '.' 42 cht
      _    <- getCHT '.' cht
      val1 <- getCHT '.' cht
      val1 `shouldBe` Just 42
    it "Cannot retrieve unknown keys" $ do
      cht <- sampleCHT
      val <- getCHT '.' cht
      val `shouldBe` Nothing
    it "All keys are kept" $ do
      cht <- newCHT
      let keys   = ['a' .. 'z']
      let pairs  = zip keys [(1 :: Int) ..]
      let putOps = map (\(k, v) -> putCHT k v cht) pairs
      sequence_ putOps
      let getOps = map (\k -> getCHT k cht) keys
      maybeVals <- sequence getOps
      case sequence maybeVals of
        Nothing   -> expectationFailure "Failed to get some of the keys"
        Just vals -> do
          let extractedPairs = zip keys vals
          extractedPairs `shouldBe` pairs
