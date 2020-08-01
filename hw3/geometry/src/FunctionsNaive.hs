module FunctionsNaive
  ( doubleArea
  , perimeter
  )
where

import Geometry (Point (..), crossProduct, len, minus)

-- | Naive implementation of polygon perimeter calculation.
perimeter :: [Point] -> Double
perimeter ps = helper 0 (last ps) ps where
  helper :: Double -> Point -> [Point] -> Double
  helper s _ []       = s
  helper s p (x : xs) = helper (s + len (x `minus` p)) x xs

-- | Naive implementation of doubled polygon area calculation.
doubleArea :: [Point] -> Int
doubleArea ps = abs $ helper 0 (last ps) ps where
  helper :: Int -> Point -> [Point] -> Int
  helper s _ []       = s
  helper s p (x : xs) = helper (s + crossProduct x p) x xs
