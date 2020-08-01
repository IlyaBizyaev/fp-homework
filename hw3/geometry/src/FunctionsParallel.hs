module FunctionsParallel
  ( doubleArea
  , perimeter
  )
where

import Control.Parallel.Strategies (rpar, runEval)

import Geometry (Point (..), crossProduct, len, minus)

-- | Pairs of adjacent list elements.
pairs :: [a] -> [(a, a)]
pairs [] = []
pairs xs = zip xs (tail xs)

-- | Parallel implementation of polygon perimeter calculation.
perimeter :: [Point] -> Double
perimeter ps = helper jobs where
  jobs :: [(Point, Point)]
  jobs = pairs $ last ps : ps
  helper :: [(Point, Point)] -> Double
  helper [] = 0
  helper js = runEval $ do
    let ~(myJobs, nextJobs) = splitAt 10000 js
    headSum <- rpar $ sum $ map (len . uncurry minus) myJobs
    tailSum <- rpar $ helper nextJobs
    return (headSum + tailSum)

-- | Parallel implementation of doubled polygon area calculation.
doubleArea :: [Point] -> Int
doubleArea ps = abs $ helper jobs where
  jobs :: [(Point, Point)]
  jobs = pairs $ last ps : ps
  helper :: [(Point, Point)] -> Int
  helper [] = 0
  helper js = runEval $ do
    let ~(myJobs, nextJobs) = splitAt 10000 js
    headSum <- rpar $ sum $ map (uncurry . flip $ crossProduct) myJobs
    tailSum <- rpar $ helper nextJobs
    return (headSum + tailSum)
