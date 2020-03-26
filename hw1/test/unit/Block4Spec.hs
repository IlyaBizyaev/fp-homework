module Block4Spec where

import           Test.Hspec
import           Block4.Task1


spec :: Spec
spec = do
  describe "Block4.Task1" $ do
    it "stringSum works" $ do
      stringSum "3 4 5" `shouldBe` Just 12
      stringSum "3   444      5555 0" `shouldBe` Just 6002
      stringSum "3 4x 5" `shouldBe` Nothing
      stringSum "Alice" `shouldBe` Nothing
      stringSum "" `shouldBe` Just 0
