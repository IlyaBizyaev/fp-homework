module Block6.Task3
  ( balancedParenthesesParser
  , integerParser
  )
where

import Block6.Task1 (Parser (..))
import Block6.Task2 (element, eof, ok, satisfy)
import Control.Applicative (Alternative (..))
import Data.Char (isDigit)

-- | Parser that consumes strings that are balanced parantheses sequences.
balancedParenthesesParser :: Parser Char ()
balancedParenthesesParser = s *> eof
  where s = (element '(' *> s *> element ')' *> s) <|> ok

-- | Parser that consumes ints with or without a sign.
integerParser :: Parser Char Int
integerParser = signP <*> digitsP where
  plusSignP  = (* 1) <$ (("" <$ element '+') <|> pure "")
  minusSignP = (* (-1)) <$ element '-'
  signP      = minusSignP <|> plusSignP

  digitsP    = (foldl addDigit 0) <$> some (satisfy isDigit)
  addDigit res c = res * 10 + (fromEnum c - fromEnum '0')
