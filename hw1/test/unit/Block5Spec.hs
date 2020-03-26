module Block5Spec where

import           Test.Hspec
import           Block5.Task1


spec :: Spec
spec = do
  describe "Block5.Task1" $ do
    -- pow, powtoneg
    let const1 = Const 5
    let const2 = Const 42
    let zeroConst = Const 0
    let negConst = Const (-10)
    it "Const expr evaluates" $ do
      eval const1 `shouldBe` Right 5
      eval const2 `shouldBe` Right 42
    it "Addition expr evaluates" $ do
      eval (Add const1 const2) `shouldBe` Right 47
      eval (Add const1 negConst) `shouldBe` Right (-5)
    it "Subtraction expr evaluates" $ do
      eval (Mul const1 const2) `shouldBe` Right 210
      eval (Mul const1 zeroConst) `shouldBe` Right 0
    it "Division expr evaluates" $ do
      eval (Div const2 const1) `shouldBe` Right 8
      eval (Div zeroConst const2) `shouldBe` Right 0
      eval (Div const2 zeroConst) `shouldBe` Left (ArithmeticError 42 0 "/")
    it "Power expr evaluates" $ do
      eval (Pow const2 const1) `shouldBe` Right 130691232
      eval (Pow const2 zeroConst) `shouldBe` Right 1
      eval (Pow const2 negConst) `shouldBe` Left (ArithmeticError 42 (-10) "^")
