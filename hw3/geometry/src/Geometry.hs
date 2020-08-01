{-# LANGUAGE InstanceSigs #-}

module Geometry
  ( Point(..)
  , Polygon
  , crossProduct
  , len
  , minus
  , plus
  , scalarProduct
  )
where

import Control.DeepSeq (NFData (..))
import Control.Monad (join)

-- | 2D point representation.
newtype Point = Point (Int, Int)

instance NFData Point where
  rnf :: Point -> ()
  rnf (Point (x, y)) = rnf x `seq` rnf y

-- | Addition of points as position vectors.
plus :: Point -> Point -> Point
plus (Point (x1, y1)) (Point (x2, y2)) = Point (x1 + x2, y1 + y2)

-- | Subtraction of points as position vectors.
minus :: Point -> Point -> Point
minus (Point (x1, y1)) (Point (x2, y2)) = Point (x1 - x2, y1 - y2)

-- | Scalar product of position vectors.
scalarProduct :: Point -> Point -> Int
scalarProduct (Point (x1, y1)) (Point (x2, y2)) = x1 * x2 + y1 * y2

-- | "Cross product" of position vectors.
crossProduct :: Point -> Point -> Int
crossProduct (Point (x1, y1)) (Point (x2, y2)) = x1 * y2 - y1 * x2

-- | Length of a position vector.
len :: Point -> Double
len = sqrt . fromIntegral . join scalarProduct

-- | Type synonym for a list of points.
type Polygon = [Point]
