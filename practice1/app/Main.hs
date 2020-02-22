module Main where

-------------------------
-- Practice 1
-------------------------


------------------------------------------
-- Implement sorting of number sequences
--
-- You should implement three different
-- sorting algorithms.
--
-- Provide implementation in `solve` function.
-- Use `words` function to split whole input to words
-- and `readInt` to convert a word to an integer.
--
-- Your program will be given a sequence
-- of integers (written to stdin).
-- First integer takes value from range [0..2] and
-- signifies sorting algorithm to use.
-- Rest of integers is a sequence that is to be sorted.
--
-- You can compile and run your program with
-- `cabal run practice1`.
--
-------------------------------


main :: IO ()
main = do
  interact solve
  putStrLn ""

readInt :: String -> Int
readInt = read

showInt :: Int -> String
showInt = show

skipOneMatch :: (Eq a) => a -> [a] -> [a]
skipOneMatch _ [] = []
skipOneMatch y (x : xs) | x == y    = xs
                        | otherwise = x : skipOneMatch y xs

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x : xs) =
  let smaller = quickSort [ a | a <- xs, a <= x ]
      bigger  = quickSort [ a | a <- xs, a > x ]
  in  smaller ++ [x] ++ bigger

selectionSort :: (Ord a) => [a] -> [a]
selectionSort [] = []
selectionSort l =
  let minimal = minimum l
      rest    = skipOneMatch minimal l
  in  minimal : selectionSort rest

bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort l =
  let
    swapPass :: (Ord a) => [a] -> (Bool, [a])
    swapPass []  = (False, [])
    swapPass [x] = (False, [x])
    swapPass (x0 : x1 : xs)
      | x0 > x1
      = (True, x1 : snd (swapPass (x0 : xs)))
      | otherwise
      = let passNext = swapPass (x1 : xs) in (fst passNext, x0 : snd passNext)
    passResult = swapPass l
  in
    if fst passResult then bubbleSort (snd passResult) else snd passResult

sort :: Int -> [Int] -> [Int]
sort algorithm numbers = case algorithm of
  0 -> quickSort numbers
  1 -> selectionSort numbers
  2 -> bubbleSort numbers
  _ -> numbers

solve :: String -> String
solve s =
  let numbers = map readInt (words s)
      sorted []       = []
      sorted (x : xs) = sort x xs
  in  unwords (map showInt (sorted numbers))
