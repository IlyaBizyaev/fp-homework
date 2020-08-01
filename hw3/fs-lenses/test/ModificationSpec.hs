module ModificationSpec where

import Data.List (sort)
import Lens.Micro ((^..), (^?))
import Test.Hspec

import Modification (descendantNames, getPath, move, removeEmptySubdir, replaceExt)
import TestData (sampleFS)
import Traversal (ls)


-- | Test advanced FS modification and full path query functions.
spec :: Spec
spec = do
  describe "Filesystem modification" $ do
    it "replaceExt only changes file extensions" $ do
      ((replaceExt "png" sampleFS) ^.. ls)
        `shouldBe` ["first", "seco.nd", "empty", "README.png", "LICENSE.png"]
    it "descendantNames works as expected" $ do
      let sortedDescendants =
            [ "42.txt"
            , "LICENSE"
            , "README.md"
            , "empty"
            , "first"
            , "seco.nd"
            , "third"
            , "virus.exe"
            ]
      sort (descendantNames sampleFS) `shouldBe` sortedDescendants
    it "removeEmptySubdir removes only empty subdirs" $ do
      ((removeEmptySubdir "empty" sampleFS) ^.. ls)
        `shouldBe` ["first", "seco.nd", "README.md", "LICENSE"]
      ((removeEmptySubdir "nonexistent" sampleFS) ^.. ls)
        `shouldBe` ["first", "seco.nd", "empty", "README.md", "LICENSE"]
      ((removeEmptySubdir "first" sampleFS) ^.. ls)
        `shouldBe` ["first", "seco.nd", "empty", "README.md", "LICENSE"]
      ((removeEmptySubdir "LICENSE" sampleFS) ^.. ls)
        `shouldBe` ["first", "seco.nd", "empty", "README.md", "LICENSE"]
  describe "Full path query" $ do
    it "move and getPath combine full path" $ do
      (sampleFS ^? move "seco.nd" . move "third" . getPath)
        `shouldBe` Just "sample/seco.nd/third/"
      (sampleFS ^? move "seco.nd" . move "third" . move "virus.exe" . getPath)
        `shouldBe` Just "sample/seco.nd/third/virus.exe"
      (sampleFS ^? move "seco.nd" . move "nonexistent" . getPath)
        `shouldBe` Nothing
