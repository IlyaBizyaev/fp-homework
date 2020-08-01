module Main where

import Control.Concurrent (threadDelay)

import Virus (Simulation (..), nextStep, setupSimulation)

-- | Clear visible portion of the ANSI terminal output.
clearTerminal :: IO ()
clearTerminal = putStr "\ESC[2J"

-- | Display a series of simulation states with output cleanup and delay.
printSteps :: [Simulation] -> IO ()
printSteps steps = do
  mapM_ (\s -> clearTerminal >> print s >> threadDelay 1000000) steps

-- | Request simulation parameters from the user and display the requested number of steps.
main :: IO ()
main = do
  putStrLn "Probability of getting infected:"
  p <- readLn :: IO Double
  putStrLn "Days for incubation:"
  dIncub <- readLn :: IO Int
  putStrLn "Days of symptoms:"
  dSympt <- readLn :: IO Int
  putStrLn "Days of immunity:"
  dImmun <- readLn :: IO Int
  putStrLn "Days to simulate:"
  n <- readLn :: IO Int

  s <- setupSimulation p dIncub dSympt dImmun
  let steps = take n $ iterate nextStep s
  printSteps steps
