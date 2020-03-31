module Block6.Task4
  ( listlistParser
  )
where

import Block6.Task1 (Parser (..))
import Block6.Task2 (element, eof, satisfy)
import Block6.Task3 (integerParser)
import Control.Applicative (Alternative (..))
import Data.Char (isDigit, isSpace)

-- | Parser that parses a list of integers of specified format into a list of
-- sublists.
-- Example: "2, 1, 10, 3, 5, -7, 2" -> [[1, 10], [5, -7, 2]].
listlistParser :: Parser Char [[Int]]
listlistParser = const <$> list <*> eof where
  whitespaceP = many (satisfy isSpace)

  digitsP     = (foldl addDigit 0) <$> some (satisfy isDigit)
  addDigit res c = res * 10 + (fromEnum c - fromEnum '0')
  nonNegIntP = (\_ x _ -> x) <$> whitespaceP <*> digitsP <*> whitespaceP
  intP       = (\_ x _ -> x) <$> whitespaceP <*> integerParser <*> whitespaceP

  ntimes :: (Alternative f) => f a -> Int -> f [a]
  ntimes _ 0 = pure []
  ntimes p n = (:) <$> p <*> ntimes p (n - 1)

  commaP           = element ','
  commaAndIntP     = flip const <$> commaP <*> intP
  sublistP         = nonNegIntP >>= ntimes commaAndIntP

  commaAndSublistP = flip const <$> commaP <*> sublistP
  list             = ((:) <$> sublistP <*> many commaAndSublistP) <|> pure []




