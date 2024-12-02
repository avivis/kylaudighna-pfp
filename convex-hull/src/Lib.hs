module Lib (
  orientation,
  sortPointsCW,
  sortPointsCCW,
  isCCWTurn,
  distFromLine2,
) where

import Data.List (sortBy)
import Linear.V2 (V2 (V2), crossZ)

isCCWTurn :: (Ord a, Num a) => V2 a -> V2 a -> V2 a -> Bool
isCCWTurn o p1 p2 = crossZ (p1 - o) (p2 - o) >= 0

squareDistance2 :: (Num a) => V2 a -> V2 a -> a
squareDistance2 (V2 x0 y0) (V2 x1 y1) = dx * dx + dy * dy
 where
  dx = x1 - x0
  dy = y1 - y0

-- Calculate distance from line
distFromLine2 :: (Num a) => V2 a -> V2 a -> V2 a -> a
distFromLine2 p0 p1 = crossZ (p1 - p0) . subtract p0

-- GT = o -> p1 -> p2 is a counter-clockwise turn
-- LT = o -> p1 -> p2 is a clockwise turn
-- EQ = o, p1, and p2 are colinear, instead compare based on distance
orientation :: (Ord a, Num a) => V2 a -> V2 a -> V2 a -> Ordering
orientation p0 p1 p2 = compare (crossZ (p1 - p0) (p2 - p0)) 0 <> compare (squareDistance2 p0 p1) (squareDistance2 p0 p2)

-- Sort a list of points in counter-clockwise order starting from the point with the lowest x value
sortPointsCW :: (Ord a, Num a) => [V2 a] -> [V2 a]
sortPointsCW [] = []
sortPointsCW points =
  let o = minimum points -- o is the minimum with respect to x, then to y
   in o : (sortBy (orientation o) . filter (/= o)) points

-- Sort a list of points in counter-clockwise order starting from the point with the lowest x value
sortPointsCCW :: (Ord a, Num a) => [V2 a] -> [V2 a]
sortPointsCCW [] = []
sortPointsCCW points =
  let o = minimum points -- o is the minimum with respect to x, then to y
   in o : (sortBy (flip (orientation o)) . filter (/= o)) points
