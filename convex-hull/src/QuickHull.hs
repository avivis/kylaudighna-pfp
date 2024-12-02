module QuickHull (quickHull2, quickHull2Par) where

import Control.DeepSeq (NFData)
import Control.Lens ((^.))
import Control.Parallel.Strategies (parList, rdeepseq, using)
import Data.Function (on)
import Data.List (maximumBy, minimumBy)
import Linear.V2 (R1 (_x), R2 (_y), V2, crossZ)

-- TODO: Handle collinear points (>=0, filter out p0)

quickHull2_ :: (Ord a, Num a) => [V2 a] -> V2 a -> V2 a -> [V2 a]
quickHull2_ points p0 p1 =
  let pointsDists = [(p, crossZ (p1 - p0) (p - p0)) | p <- points]
      onLeftDists = filter ((> 0) . snd) pointsDists
      onLeft = map fst onLeftDists
   in if length onLeft < 2
        then p0 : onLeft
        else
          let pm = fst $ maximumBy (compare `on` snd) onLeftDists
           in quickHull2_ onLeft p0 pm ++ quickHull2_ onLeft pm p1

-- Any shape with < 4 points is automatically its own convex hull, as it takes 3 points to make a 2D simplex
quickHull2 :: (Ord a, Num a) => [V2 a] -> [V2 a]
quickHull2 [] = []
quickHull2 p@[_] = p
quickHull2 p@[_, _] = p
quickHull2 p@[_, _, _] = p
quickHull2 points =
  let maxXPoint = maximumBy (compare `on` (^. _x)) points
      minXPoint = minimumBy (compare `on` (^. _x)) points
   in quickHull2_ points minXPoint maxXPoint ++ quickHull2_ points maxXPoint minXPoint

-- Min X, min y, max X and max Y points all have to be part of the convex hull, so I do a 4-way
-- parallel approach here
quickHull2Par :: (Num a, Ord a, NFData a) => [V2 a] -> [V2 a]
quickHull2Par [] = []
quickHull2Par p@[_] = p
quickHull2Par p@[_, _] = p
quickHull2Par p@[_, _, _] = p
quickHull2Par points =
  let maxDepth = 100 -- TODO: How do we determine maxDepth?
      _quickHull2Par :: (Num a, Ord a, NFData a) => Int -> [V2 a] -> (V2 a, V2 a) -> [V2 a]
      _quickHull2Par d ps (p0, p1)
        | null onLeft = [p0]
        | d < maxDepth = concat (map (_quickHull2Par (d + 1) onLeft) nextLines `using` parList rdeepseq)
        | otherwise = concatMap (_quickHull2Par (d + 1) onLeft) nextLines
       where
        onLeftDists = filter ((> 0) . snd) [(p, crossZ (p1 - p0) (p - p0)) | p <- ps]
        onLeft = map fst onLeftDists
        pm = fst $ maximumBy (compare `on` snd) onLeftDists
        nextLines = [(p0, pm), (pm, p1)]

      maxXPoint = maximumBy (compare `on` (^. _x)) points
      minXPoint = minimumBy (compare `on` (^. _x)) points
      maxYPoint = maximumBy (compare `on` (^. _y)) points
      minYPoint = minimumBy (compare `on` (^. _y)) points
      --
      topLeft = (minXPoint, maxYPoint)
      topRight = (maxYPoint, maxXPoint)
      bottomRight = (maxXPoint, minYPoint)
      bottomLeft = (minYPoint, minXPoint)
   in concat (map (_quickHull2Par 1 points) [topLeft, topRight, bottomRight, bottomLeft] `using` parList rdeepseq)

-- -- The cross product of (p1 - p0) and (p2 - p0) should be positive!
