module Lib (
  comparePointsPolar,
  sortPointsCW,
  sortPointsCCW,
  isCCWTurn,
) where

import Data.List (sortBy)
import Linear.Metric (distance)
import Linear.V2 (V2, crossZ)

isCCWTurn :: (Ord a, Floating a) => V2 a -> V2 a -> V2 a -> Bool
isCCWTurn o p1 p2 = crossZ (p1 - o) (p2 - o) >= 0

-- GT = o -> p1 -> p2 is a counter-clockwise turn
-- LT = o -> p1 -> p2 is a clockwise turn
-- EQ = o, p1, and p2 are colinear, instead compare based on distance
comparePointsPolar :: (Ord a, Floating a) => V2 a -> V2 a -> V2 a -> Ordering
comparePointsPolar o p1 p2 = compare (crossZ (p1 - o) (p2 - o)) 0 <> compare (distance o p1) (distance o p2)

-- Sort a list of points in counter-clockwise order starting from the point with the lowest x value
sortPointsCW :: (Ord a, Floating a) => [V2 a] -> [V2 a]
sortPointsCW [] = []
sortPointsCW points =
  let o = minimum points -- o is the minimum with respect to x, then to y
   in o : (sortBy (comparePointsPolar o) . filter (/= o)) points
-- Sort a list of points in counter-clockwise order starting from the point with the lowest x value
sortPointsCCW :: (Ord a, Floating a) => [V2 a] -> [V2 a]
sortPointsCCW [] = []
sortPointsCCW points =
  let o = minimum points -- o is the minimum with respect to x, then to y
   in o : (sortBy (flip (comparePointsPolar o)) . filter (/= o)) points
