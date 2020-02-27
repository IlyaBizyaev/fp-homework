module Task4
  ( factorial
  , fibonacci
  , iterateElement
  , mapFix
  ) where

import Data.Function (fix)

-- | Construct an infinite list of repeating element x.
iterateElement :: a -> [a]
iterateElement x = fix (x :)

-- | Calculate the nth Fibonacci number.
fibonacci :: Integer -> Integer
fibonacci =
  let fib f n | n > 1     = f (n - 1) + f (n - 2)
              | n < 0     = (-1) ^ (-n + 1) * f (-n)
              | otherwise = n
  in  fix fib

-- | Calculate the factorial of n.
factorial :: Integer -> Integer
factorial =
  let fact f n | n > 1     = n * f (n - 1)
               | n < 0     = undefined
               | otherwise = 1
  in  fix fact

-- | 'map' function implemented with the fixed point combinator.
mapFix :: (a -> b) -> [a] -> [b]
mapFix = fix (\m f xs -> if null xs then [] else f (head xs) : m f (tail xs))
