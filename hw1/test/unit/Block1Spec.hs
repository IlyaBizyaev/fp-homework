module Block1Spec where

import Block1.Task1
import Block1.Task2
import Block1.Task3
import Control.Exception (evaluate)
import Test.Hspec

spec :: Spec
spec = do
  describe "Block1.Task1" $ do
    it "nextDay returns next day of the week" $ do
      nextDay Monday `shouldBe` Tuesday
      nextDay Sunday `shouldBe` Monday
    it "afterDays returns day that will happen after current in n days" $ do
      afterDays 3 Monday `shouldBe` Thursday
      afterDays 2 Saturday `shouldBe` Monday
    it "isWeekend returns true for and only for weekends" $ do
      isWeekend Thursday `shouldBe` False
      isWeekend Sunday `shouldBe` True
    it "daysToParty returns days remaining till Friday" $ do
      daysToParty Wednesday `shouldBe` 2
      daysToParty Friday `shouldBe` 0
      daysToParty Saturday `shouldBe` 6

  describe "Block1.Task2" $ do
    let makeNat x = fromInteger x :: Nat
    it "Addition of Nat works" $ do
      S (S Z) + S (S (S Z)) `shouldBe` S (S (S (S (S Z))))
      makeNat 3 + makeNat 5 `shouldBe` makeNat 8
      makeNat 0 + makeNat 0 `shouldBe` makeNat 0
    it "Multiplication of Nat works" $ do
      makeNat 3 * makeNat 5 `shouldBe` makeNat 15
      makeNat 9 * makeNat 0 `shouldBe` makeNat 0
    it "Subtraction of Nat works" $ do
      makeNat 8 - makeNat 5 `shouldBe` makeNat 3
      evaluate (makeNat 5 - makeNat 8) `shouldThrow` anyErrorCall
    it "fromInteger works" $ do
      fromInteger 4 `shouldBe` S (S (S (S Z)))
      fromInteger 0 `shouldBe` Z
    it "natToInteger works" $ do
      natToInteger (S (S Z)) `shouldBe` 2
      natToInteger Z `shouldBe` 0
    it "Equality implementation for Nat is sane" $ do
      S (S (S Z)) `shouldBe` S (S (S Z))
      S (S (S Z)) `shouldNotBe` S Z
    it "Comparison of Nat works" $ do
      S Z `shouldSatisfy` (< S (S Z))
      S Z `shouldNotSatisfy` (>= S (S Z))
      S (S Z) `shouldSatisfy` (>= Z)
    it "isEven works" $ do
      makeNat 10 `shouldSatisfy` isEven
      makeNat 0 `shouldSatisfy` isEven
      makeNat 11 `shouldNotSatisfy` isEven

  describe "Block1.Task3" $ do
    -- fromList, delete
    let searchTreeSample = STNode 'x' 3 STLeaf (STNode 'y' 1 STLeaf STLeaf)
    it "isEmpty works" $ do
      (STLeaf :: SearchTree Float) `shouldSatisfy` isEmpty
      STNode 'x' 3 STLeaf STLeaf `shouldNotSatisfy` isEmpty
    it "size works" $ do
      size (STLeaf :: SearchTree Float) `shouldBe` 0
      size searchTreeSample `shouldBe` 4
    it "search works" $ do
      find (STLeaf :: SearchTree Float) 42.0 `shouldBe` False
      find searchTreeSample 'x' `shouldBe` True
      find searchTreeSample 'y' `shouldBe` True
      find searchTreeSample 'z' `shouldBe` False
    it "insert works" $ do
      insert (STLeaf :: SearchTree Float) 42.0
        `shouldBe` STNode 42.0 1 STLeaf STLeaf
      insert (STNode 'x' 3 STLeaf STLeaf) 'y' `shouldBe` searchTreeSample
    it "fromList works" $ do
      fromList ([5, 3, 5, 4] :: [Integer])
        `shouldBe` STNode 5
                          2
                          (STNode 3 1 STLeaf (STNode 4 1 STLeaf STLeaf))
                          STLeaf
    it "delete works" $ do
      delete searchTreeSample 'y' `shouldBe` (True, STNode 'x' 3 STLeaf STLeaf)
      delete searchTreeSample 'x'
        `shouldBe` (True, STNode 'x' 2 STLeaf (STNode 'y' 1 STLeaf STLeaf))
      delete searchTreeSample 'z' `shouldBe` (False, searchTreeSample)
      delete (STLeaf :: SearchTree Char) 'x'
        `shouldBe` (False, STLeaf :: SearchTree Char)

