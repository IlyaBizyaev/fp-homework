module Block5.Task2
  ( moving
  )
where

import Control.Monad.State

-- | An implementation of Simple Moving Average algorithm, based on State monad.
moving :: Int -> [Double] -> [Double]
moving n l | n <= 0    = error "Expected a positive window size"
           | otherwise = reverse $ evalState (movingMonad n l) ([], []) where
  movingMonad :: Int -> [Double] -> State ([Double], [Double]) [Double]
  movingMonad _ [] = do
    (result, _) <- get
    return result

  movingMonad limit (x : xs) = do
    (result, window) <- get
    let newWindow = take limit (x : window)
    let newResult = (sum newWindow / fromIntegral (length newWindow)) : result
    put (newResult, newWindow)
    movingMonad limit xs
