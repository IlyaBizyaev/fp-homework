{-# LANGUAGE InstanceSigs #-}

module Virus
  ( Simulation(..)
  , setupSimulation
  , nextStep
  )
where

import Control.Comonad (extend, extract)
import Data.List (intercalate)
import System.Random (StdGen, getStdRandom, mkStdGen, random)

import Grid (Grid (..), down, gridWrite, left, right, up)
import ListZipper (ListZipper (..), genericMove, iterateTail, toList)

-- | Data type for person's infection state in the simulation.
data PersonState = Susceptible | Incubating Int | Symptoms Int | Immune Int

instance Show PersonState where
  show :: PersonState -> String
  show Susceptible    = "_"
  show (Incubating _) = "*"
  show (Symptoms   _) = "#"
  show (Immune     _) = "@"

-- | Record for a person in the simulation.
data Person = Person { pState :: PersonState, pRng :: StdGen }

instance Show Person where
  show :: Person -> String
  show = show . pState

-- | Number of people to the sides of the original infected person to display.
viewportLimit :: Int
viewportLimit = 10

-- | Record for the simulation that stores its state and parameters.
data Simulation = Simulation { sState          :: Grid Person
                             , sProbability    :: Double
                             , sDaysIncubation :: Int
                             , sDaysSymptoms   :: Int
                             , sDaysImmune     :: Int
                             }

instance Show Simulation where
  show :: Simulation -> String
  show s = intercalate "\n" rowsShown   where
    rows      = unGrid . sState $ s
    rowsList  = toList viewportLimit $ fmap (toList viewportLimit) rows
    rowsShown = map (concatMap show) rowsList

-- | Provided with simulation parameters, instantiate a simulation with initial state.
setupSimulation :: Double -> Int -> Int -> Int -> IO Simulation
setupSimulation p daysIncub daysSympt daysImmune = do
  seed <- getStdRandom random :: IO Int
  let initialPerson = Person Susceptible (mkStdGen seed)
  let mainRow = LZ (iterateTail leftTweak initialPerson)
                   initialPerson
                   (iterateTail rightTweak initialPerson)
  let state = Grid $ genericMove upTweak downTweak mainRow
  let stateWithIncubating =
        gridWrite (initialPerson { pState = Incubating daysIncub }) state

  return $ Simulation { sState          = stateWithIncubating
                      , sProbability    = p
                      , sDaysIncubation = daysIncub
                      , sDaysSymptoms   = daysSympt
                      , sDaysImmune     = daysImmune
                      } where
  tweakPersonRng :: (Int -> Int) -> Person -> Person
  tweakPersonRng f (Person state rng) = Person state newRng   where
    (newSeed, _) = random rng
    newRng       = mkStdGen (f newSeed)
  leftTweak  = tweakPersonRng (* (-1))
  rightTweak = tweakPersonRng (^ (2 :: Int))
  upTweak    = fmap (tweakPersonRng (* 3))
  downTweak  = fmap (tweakPersonRng (`div` 2))

-- | Advance the simulation by a step ("day").
nextStep :: Simulation -> Simulation
nextStep s = s { sState = extend rule (sState s) } where
  transmitsVirus :: PersonState -> Bool
  transmitsVirus (Symptoms   _) = True
  transmitsVirus (Incubating _) = True
  transmitsVirus _              = False

  anyInfected :: [Person] -> Bool
  anyInfected = any (transmitsVirus . pState)

  neighboursTransmit :: Grid Person -> Bool
  neighboursTransmit g = anyInfected
    $ map (\direction -> extract $ direction g) [left, right, up, down]

  infectIfUnlucky :: Grid Person -> Person
  infectIfUnlucky grid = if (neighboursTransmit grid && chance <= p)
    then infectedPerson
    else luckyPerson   where
    p                = sProbability s
    oldRng           = pRng $ extract grid
    (chance, newRng) = random oldRng
    infectedPerson   = Person (Incubating (sDaysIncubation s)) newRng
    luckyPerson      = Person Susceptible newRng

  rule :: Grid Person -> Person
  rule grid = after   where
    Person beforeState beforeRng = extract grid
    after                        = case beforeState of
      Incubating 1 -> Person (Symptoms (sDaysSymptoms s)) beforeRng
      Symptoms   1 -> Person (Immune (sDaysImmune s)) beforeRng
      Immune     1 -> Person (Susceptible) beforeRng
      Incubating n -> Person (Incubating (n - 1)) beforeRng
      Symptoms   n -> Person (Symptoms (n - 1)) beforeRng
      Immune     n -> Person (Immune (n - 1)) beforeRng
      Susceptible  -> infectIfUnlucky grid
