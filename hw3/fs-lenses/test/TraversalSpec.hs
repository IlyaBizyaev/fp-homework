module TraversalSpec where

import Lens.Micro ((^..), (^?))
import Test.Hspec

import TestData (sampleFS)
import Traversal (cd, file, ls)

-- | Ensure that file traversal util functions (cd, file, ls) work correctly.
spec :: Spec
spec = do
  describe "Directory navigation" $ do
    it "File existence check works as expected" $ do
      let f   = sampleFS ^? cd "seco.nd" . cd "third" . file "virus.exe"
      let noF = sampleFS ^? cd "a" . cd "b" . file "c"
      f `shouldBe` Just "virus.exe"
      noF `shouldBe` Nothing
    it "Directory enumeration works as expected" $ do
      sampleFS
        ^..        ls
        `shouldBe` ["first", "seco.nd", "empty", "README.md", "LICENSE"]
