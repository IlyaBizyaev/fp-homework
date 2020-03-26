{-# LANGUAGE InstanceSigs #-}

module Block2.Task2
  ( splitOn
  , joinWith
  )
where

import Data.List.NonEmpty (NonEmpty (..), fromList)

splitOn :: (Eq a) => a -> [a] -> NonEmpty [a]
splitOn on toSplit = fromList . reverse $ reverse lastElement : currentList where
  (currentList, lastElement) = foldl (f on) ([], []) toSplit
  f :: (Eq a) => a -> ([[a]], [a]) -> a -> ([[a]], [a])
  f splitBy (curList, lastElem) x
    | x == splitBy = (reverse lastElem : curList, [])
    | otherwise    = (curList, x : lastElem)

joinWith :: a -> NonEmpty [a] -> [a]
joinWith with (x :| xs) = x ++ foldr (\el result -> with : el ++ result) [] xs
