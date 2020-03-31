{-# LANGUAGE InstanceSigs #-}

module Block6.Task1
  ( Parser(..)
  )
where

import Control.Applicative (Alternative (..))

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

-- | Parser combinator type.
newtype Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
  fmap g (Parser f) = Parser $ fmap (first g) . f

instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure a = Parser $ \s -> Just (a, s)

  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  Parser pf <*> Parser pa = Parser $ \s -> do
    (a, r) <- pf s
    (b, q) <- pa r
    Just (a b, q)

instance Monad (Parser s) where
  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  Parser pa >>= pf = Parser $ \s -> do
    (a, r) <- pa s
    runParser (pf a) r

instance Alternative (Parser s) where
  empty :: Parser s a
  empty = Parser $ \_ -> Nothing
  (<|>) :: Parser s a -> Parser s a -> Parser s a
  Parser pa <|> Parser pb = Parser $ \s -> pa s <|> pb s
