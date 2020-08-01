module Block6.Task2
  ( element
  , eof
  , ok
  , satisfy
  , stream
  )
where

import Block6.Task1 (Parser (..))

-- | Parser that always succeeds without consuming any input.
ok :: Parser s ()
ok = Parser $ \s -> Just ((), s)

-- | Parser that only succeeds at the end of the input stream.
eof :: Parser s ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

-- | Parser that consumes a single character and returns it
-- if the predicate is true.
satisfy :: (a -> Bool) -> Parser a a
satisfy p = Parser $ \s -> case s of
  []       -> Nothing
  (x : xs) -> if p x then Just (x, xs) else Nothing

-- | Parser that consumes the given character and returns it.
element :: Eq a => a -> Parser a a
element c = satisfy (== c)

-- | Parser that consumes the given string and returns it.
stream :: Eq a => [a] -> Parser a [a]
stream []       = Parser $ \s -> Just ([], s)
stream (x : xs) = (:) <$> element x <*> stream xs
