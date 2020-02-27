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
fibonacci = fix (\fib n -> if n > 1 then fib (n - 1) + fib (n - 2) else n)

-- | Calculate the factorial of n.
factorial :: Integer -> Integer
factorial = fix (\fact n -> if n > 1 then n * fact (n - 1) else 1)

-- | 'map' function implemented with the fixed point combinator.
mapFix :: (a -> b) -> [a] -> [b]
mapFix = fix (\m f xs -> if null xs then [] else f (head xs) : m f (tail xs))
