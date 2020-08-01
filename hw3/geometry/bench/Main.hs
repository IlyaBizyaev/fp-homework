module Main where

import Criterion.Main
import Data.List (iterate')

import qualified FunctionsNaive as N
import qualified FunctionsParallel as P
import Geometry (Point (..), Polygon)

-- | Generate regular n-gon with center at (x, y) and radius r.
nCircleDots :: Point -> Double -> Int -> [Point]
nCircleDots (Point (x, y)) r n | n <= 0    = []
                               | otherwise = map dotByAngle angles where
  inc :: Double
  inc = 2 * pi / fromIntegral n
  dotByAngle :: Double -> Point
  dotByAngle a = Point (x + round (r * cos a), y + round (r * sin a))
  angles :: [Double]
  angles = take n $ iterate' (+ inc) 0

-- | Instantiate test shapes.
setupEnv :: IO (Polygon, Polygon)
setupEnv = do
  let testPolygon1 = map Point [(3, 2), (6, 2), (6, 5), (3, 5)]
  let testPolygon2 = nCircleDots (Point (3, 2)) 300 (10 ^ (7 :: Int))
  return (testPolygon1, testPolygon2)

-- | Perform a series of benchmarks for perimeter and area using 2 shapes and 2 implementations.
main :: IO ()
main = defaultMain
  [ env setupEnv $ \ ~(testPolygon1, testPolygon2) -> bgroup
      "main"
      [ bgroup
        "perimeter1"
        [ bench "naive" $ whnf N.perimeter testPolygon1
        , bench "parallel" $ whnf P.perimeter testPolygon1
        ]
      , bgroup
        "perimeter2"
        [ bench "naive" $ whnf N.perimeter testPolygon2
        , bench "parallel" $ whnf P.perimeter testPolygon2
        ]
      , bgroup
        "area1"
        [ bench "naive" $ whnf N.doubleArea testPolygon1
        , bench "parallel" $ whnf P.doubleArea testPolygon1
        ]
      , bgroup
        "area2"
        [ bench "naive" $ whnf N.doubleArea testPolygon2
        , bench "parallel" $ whnf P.doubleArea testPolygon2
        ]
      ]
  ]

