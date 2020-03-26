module Block3Spec where

import Block3.Task1
import Block3.Task2
import Test.Hspec

data Sum a = Sum { getSum :: a } deriving Eq

instance Num n => Semigroup (Sum n) where
  Sum x <> Sum y = Sum (x + y)

instance Num n => Monoid (Sum n) where
  mempty = Sum 0

instance Show n => Show (Sum n) where
  show x = show (getSum x)


spec :: Spec
spec = do
  describe "Block3.Task1" $ do
    it "maybeConcat works" $ do
      maybeConcat [Just [1, 2, 3], Nothing, Just [4, 5]]
        `shouldBe` ([1, 2, 3, 4, 5] :: [Integer])
      maybeConcat ([] :: [Maybe [Float]]) `shouldBe` []
      maybeConcat ([Nothing, Nothing] :: [Maybe [Char]]) `shouldBe` []
      maybeConcat ([Just [8], Just [4]] :: [Maybe [Int]]) `shouldBe` [8, 4]
    it "eitherConcat works" $ do
      eitherConcat [Left (Sum (3)), Right [1, 2, 3], Left (Sum 5), Right [4, 5]]
        `shouldBe` (Sum { getSum = (8 :: Int) }, [1, 2, 3, 4, 5] :: [Int])
      eitherConcat [Left (Just "x"), Left Nothing, Right (Just "y")]
        `shouldBe` (Just "x", Just "y")

  describe "Block3.Task2" $ do
    it "Semigroup for NonEmpty satisfies the law" $ do
      (((1 :: Int) :| [2, 3]) <> (4 :| [5, 6]))
        <>         (7 :| [8, 9])
        `shouldBe` (1 :| [2, 3])
        <>         ((4 :| [5, 6]) <> (7 :| [8, 9]))
    it "Semigroup for ThisOrThat satisfies the law" $ do
      (This 'x' <> That 'y')
        <>         Both 'n' 'm'
        `shouldBe` This 'x'
        <>         (That 'y' <> Both 'n' 'm')
    it "Semigroup for Name behaves like in the task" $ do
      Name "root" <> Name "server" `shouldBe` Name "root.server"
    it "Semigroup for Name satisfies the law" $ do
      (Name "x" <> Name "y")
        <>         Name "z"
        `shouldBe` Name "x"
        <>         (Name "y" <> Name "z")
    it "Monoid for Name satisfies the laws" $ do
      Name "server" <> mempty `shouldBe` Name "server"
      mempty <> Name "server" `shouldBe` Name "server"
      mconcat [Name "root", Name "server"]
        `shouldBe` foldr (<>) mempty [Name "root", Name "server"]
    -- Testing Endo here is not done because we cannot implement Eq and Show


