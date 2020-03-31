module Block6Spec where

import Block6.Task1 (Parser (..))
import Block6.Task2
import Block6.Task3
import Block6.Task4
import Control.Applicative (Alternative (..), liftA2)
import Data.Char (isDigit, isSpace)
import Test.Hspec

ntimes :: (Alternative f) => f a -> Int -> f [a]
ntimes _ 0 = pure []
ntimes p n = (:) <$> p <*> ntimes p (n - 1)

spec :: Spec
spec = do
  describe "Block6.Task1" $ do
    it "Functor instance works as expected" $ do
      let p = pure (42 :: Int)
      runParser (fmap ((+ 2) . (* 4)) p) "any"
        `shouldBe` runParser ((fmap (+ 2) . fmap (* 4)) p) "any"
      runParser (fmap (const 'z') (pure (42 :: Int))) "f"
        `shouldBe` Just ('z', "f")
    it "Applicative instance works as expected" $ do
      let pa = pure (const 'z')
      let pb = pure (42 :: Int)
      runParser (pa <*> pb) "any" `shouldBe` runParser (liftA2 id pa pb) "any"
      runParser pb "meaning" `shouldBe` Just (42, "meaning")
      runParser (pa <*> pb) "cat" `shouldBe` Just ('z', "cat")
    it "Monad instance works as expected" $ do
      let digitP = (\c -> fromEnum c - fromEnum '0') <$> satisfy isDigit
      runParser (digitP >>= ntimes digitP) "1234" `shouldBe` Just ([2], "34")
    it "Alternative instance works as expected" $ do
      let p = element 'c' <|> element 'h'
      runParser p "cat" `shouldBe` Just ('c', "at")
      runParser p "hello" `shouldBe` Just ('h', "ello")
      runParser p "zsh" `shouldBe` Nothing
      runParser (empty :: Parser Char Char) "hello" `shouldBe` Nothing
  describe "Block6.Task2" $ do
    it "ok parser works" $ do
      runParser ok "hello" `shouldBe` Just ((), "hello")
      runParser ok "" `shouldBe` Just ((), "")
    it "eof parser works" $ do
      runParser eof "hello" `shouldBe` Nothing
      runParser eof "" `shouldBe` Just ((), "")
    it "satisfy parser works" $ do
      runParser (satisfy isDigit) "hello" `shouldBe` Nothing
      runParser (satisfy isSpace) " " `shouldBe` Just (' ', "")
      runParser (satisfy isDigit) "42hello" `shouldBe` Just ('4', "2hello")
    it "element parser works" $ do
      runParser (element 'h') "hello" `shouldBe` Just ('h', "ello")
      runParser (element 'z') "hello" `shouldBe` Nothing
      runParser (element 'h') "" `shouldBe` Nothing
    it "stream parser works" $ do
      runParser (stream "he") "hello" `shouldBe` Just ("he", "llo")
      runParser (stream "up") "hello" `shouldBe` Nothing
      runParser (stream "he") "" `shouldBe` Nothing
  describe "Block6.Task3" $ do
    it "balancedParenthesesParser works" $ do
      runParser balancedParenthesesParser "()" `shouldBe` Just ((), "")
      runParser balancedParenthesesParser "(()()())" `shouldBe` Just ((), "")
      runParser balancedParenthesesParser "(((())))" `shouldBe` Just ((), "")
      runParser balancedParenthesesParser "((())" `shouldBe` Nothing
    it "integerParser works" $ do
      runParser integerParser "123" `shouldBe` Just (123, "")
      runParser integerParser "+123" `shouldBe` Just (123, "")
      runParser integerParser "-123" `shouldBe` Just (-123, "")
      runParser integerParser "0" `shouldBe` Just (0, "")
      runParser integerParser "24h" `shouldBe` Just (24, "h")
      runParser integerParser "h" `shouldBe` Nothing
  describe "Block6.Task4" $ do
    it "listlistParser works" $ do
      runParser listlistParser "" `shouldBe` Just ([], "")
      runParser listlistParser "0" `shouldBe` Just ([[]], "")
      runParser listlistParser "0,  0" `shouldBe` Just ([[], []], "")
      runParser listlistParser "1  ,42" `shouldBe` Just ([[42]], "")
      runParser listlistParser "2, 1,+10  , 3,5,-7, 2"
        `shouldBe` Just ([[1, 10], [5, -7, 2]], "")
      runParser listlistParser "0  0" `shouldBe` Nothing
      runParser listlistParser ",," `shouldBe` Nothing
      runParser listlistParser "1, 3  1, 42" `shouldBe` Nothing
      runParser listlistParser "-2, 3, 24" `shouldBe` Nothing
