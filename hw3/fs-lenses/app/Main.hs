module Main where

import System.Environment (getArgs)

import FS (getDirectory)

-- | Read structure of the directory from the first argument into memory and display it as a tree.
main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> error "Please provide an argument"
    (a : _) -> do
      fs <- getDirectory a
      print fs
