module PracticeSpec where

import Lens.Micro ((^.), (^?))
import Test.Hspec

import FS (FS (..))
import Practice (addRootNameSuffix, contents, dirChildren, dirFilenames, dirName, fileName,
                 firstSubdirectory, name, renameToRoot, _Dir, _File)
import TestData (rootFileFS, sampleFS)

-- | Ensure that basic FS access and modification lenses, traversals and functions work correctly.
spec :: Spec
spec = do
  describe "Basic access" $ do
    it "_Dir traverses only directories" $ do
      sampleFS ^? _Dir `shouldBe` Just sampleFS
      rootFileFS ^? _Dir `shouldBe` Nothing
    it "_File traverses only files" $ do
      rootFileFS ^? _File `shouldBe` Just rootFileFS
      sampleFS ^? _File `shouldBe` Nothing
    it "name gets both dir and file names" $ do
      sampleFS ^. name `shouldBe` "sample"
      rootFileFS ^. name `shouldBe` "rootFile"
    it "contents is not Nothing only for directories" $ do
      sampleFS ^? contents `shouldBe` Just (_contents sampleFS)
      rootFileFS ^? contents `shouldBe` Nothing
  describe "Combined access" $ do
    it "dirName gets only directory names" $ do
      dirName sampleFS `shouldBe` Just "sample"
      dirName rootFileFS `shouldBe` Nothing
    it "fileName gets only file names" $ do
      fileName rootFileFS `shouldBe` "rootFile"
      fileName sampleFS `shouldBe` ""
    it "dirChildren returns dir children for dirs and [] for files" $ do
      dirChildren sampleFS `shouldBe` _contents sampleFS
      dirChildren rootFileFS `shouldBe` []
    it "dirFilenames return dir children filenames non-recursively" $ do
      dirFilenames sampleFS `shouldBe` ["README.md", "LICENSE"]
      dirFilenames rootFileFS `shouldBe` []
    it "firstSubdirectory returns name of first subdir, if exists" $ do
      firstSubdirectory sampleFS `shouldBe` Just "first"
      firstSubdirectory rootFileFS `shouldBe` Nothing
  describe "Modification" $ do
    it "renameToRoot always changes name of root object to /" $ do
      renameToRoot sampleFS `shouldBe` (sampleFS { _name = "/" })
      renameToRoot rootFileFS `shouldBe` (rootFileFS { _name = "/" })
    it "addRootNameSuffix always adds given suffix to root object name" $ do
      addRootNameSuffix "42" sampleFS
        `shouldBe` (sampleFS { _name = "sample42" })
      addRootNameSuffix "42" rootFileFS
        `shouldBe` (rootFileFS { _name = "rootFile42" })
