module Block2Spec where

import Block2.Task2
import Data.List.NonEmpty (fromList)
import Test.Hspec


spec :: Spec
spec = do
  describe "Block2.Task2" $ do
    it "splitOn works" $ do
      splitOn '/' "path/to/file" `shouldBe` fromList ["path", "to", "file"]
      splitOn '/' "cat" `shouldBe` fromList ["cat"]
      splitOn '/' "" `shouldBe` fromList [""]
      splitOn '/' "path/" `shouldBe` fromList ["path", ""]
    it "joinWith works" $ do
      joinWith '/' (fromList ["path", "to", "file"]) `shouldBe` "path/to/file"
      joinWith '/' (fromList ["cat"]) `shouldBe` "cat"
      joinWith '/' (fromList [""]) `shouldBe` ""
      joinWith '/' (fromList ["path", ""]) `shouldBe` "path/"
