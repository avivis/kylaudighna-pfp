module Lib (
  comparePointsPolar,
  sortPointsCCW,
  isCCWTurn,
) where

import Data.List (sortBy)
import Linear.Metric (distance)
import Linear.V2 (V2, crossZ)

isCCWTurn :: (Ord a, Floating a) => V2 a -> V2 a -> V2 a -> Bool
isCCWTurn o p1 p2 = crossZ (p1 - o) (p2 - o) >= 0

comparePointsPolar :: (Ord a, Floating a) => V2 a -> V2 a -> V2 a -> Ordering
comparePointsPolar o p1 p2 = compare (crossZ (p1 - o) (p2 - o)) 0 <> compare (distance o p1) (distance o p2)

sortPointsCCW :: (Ord a, Floating a) => [V2 a] -> [V2 a]
sortPointsCCW [] = []
sortPointsCCW ps =
  let o = minimum ps -- o is the minimum with respect to x, then to y
   in o : (sortBy (comparePointsPolar o) . filter (/= o)) ps
