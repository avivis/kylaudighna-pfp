module QuickHull (quickHull2, quickHull2Par) where

import Control.DeepSeq (NFData)
import Control.Lens ((^.))
import Control.Parallel.Strategies (Strategy, parMap, parTuple2, rdeepseq, using)
import Data.Function (on)
import Data.List (maximumBy, partition)
import Linear.V2 (R1 (_x), R2 (_y), V2, crossZ)

-- import Linear.V3 (V3 (..), cross)

-- TODO: Create partition before calling quickHull_
-- TODO: Handle collinear points (>=0, filter out p0)

parConcatMap :: Strategy [b] -> (a -> [b]) -> [a] -> [b]
parConcatMap strat f l = concat (parMap strat f l)

distFromLine :: (Num a) => V2 a -> V2 a -> V2 a -> a
distFromLine p0 p1 = crossZ (p1 - p0) . subtract p0

-- Any shape with < 4 points is automatically its own convex hull, as it takes 3 points to make a 2D simplex
quickHull2 :: (Ord a, Num a) => [V2 a] -> [V2 a]
quickHull2 [] = []
quickHull2 p@[_] = p
quickHull2 p@[_, _] = p
quickHull2 p@[_, _, _] = p
quickHull2 points =
  let
    _quickHull2 :: (Num a, Ord a) => [V2 a] -> V2 a -> V2 a -> [V2 a]
    _quickHull2 ps p0 p1
      | (null . drop 1) ps = p0 : ps
      | otherwise = _quickHull2 onLeft p0 pm ++ _quickHull2 onRight pm p1
     where
      pm = maximumBy (compare `on` distFromLine p0 p1) ps
      (onLeft, onRightOrCenter) = partition ((> 0) . distFromLine p0 pm) ps
      onRight = filter ((> 0) . distFromLine pm p1) onRightOrCenter

    pXMax = maximum points
    pXMin = minimum points

    (topPoints, bottomPoints) = partition ((> 0) . distFromLine pXMin pXMax) points
   in
    _quickHull2 topPoints pXMin pXMax ++ _quickHull2 bottomPoints pXMax pXMin

-- Min X, min y, max X and max Y points all have to be part of the convex hull, so I do a 4-way
-- parallel approach here
quickHull2Par :: (Num a, Ord a, NFData a) => [V2 a] -> [V2 a]
quickHull2Par [] = []
quickHull2Par p@[_] = p
quickHull2Par p@[_, _] = p
quickHull2Par p@[_, _, _] = p
quickHull2Par points =
  let _quickHull2Par :: (Num a, Ord a, NFData a) => [V2 a] -> V2 a -> V2 a -> [V2 a]
      _quickHull2Par ps p0 p1
        | (null . drop 1) ps = p0 : ps
        | otherwise = l ++ r
       where
        pm = maximumBy (compare `on` distFromLine p0 p1) ps
        (onLeft, onRight) = (filter ((> 0) . distFromLine p0 pm) ps, filter ((> 0) . distFromLine pm p1) ps) `using` parTuple2 rdeepseq rdeepseq
        (l, r) = (_quickHull2Par onLeft p0 pm, _quickHull2Par onRight pm p1) `using` parTuple2 rdeepseq rdeepseq


      (pXMax, pXMin) = (maximum points, minimum points) `using` parTuple2 rdeepseq rdeepseq
      (topPoints, bottomPoints) = partition ((> 0) . distFromLine pXMin pXMax) points
      (leftHull, rightHull) = (_quickHull2Par topPoints pXMin pXMax, _quickHull2Par bottomPoints pXMax pXMin) `using` parTuple2 rdeepseq rdeepseq
      -- maxXPoint = maximumBy (compare `on` (^. _x)) points
      -- minXPoint = minimumBy (compare `on` (^. _x)) points
      -- maxYPoint = maximumBy (compare `on` (^. _y)) points
      -- minYPoint = minimumBy (compare `on` (^. _y)) points
      -- topLeft = (minXPoint, maxYPoint)
      -- topRight = (maxYPoint, maxXPoint)
      -- bottomRight = (maxXPoint, minYPoint)
      -- bottomLeft = (minYPoint, minXPoint)
      -- in parConcatMap rdeepseq (_quickHull2Par 1 points) [topLeft, topRight, bottomRight, bottomLeft]

     in leftHull ++ rightHull
