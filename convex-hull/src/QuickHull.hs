module QuickHull (quickHull2, quickHull2Par) where

import Control.DeepSeq (NFData, force)
import Control.Lens ((^.))
import Control.Parallel.Strategies (rpar, rseq, runEval)
import Data.Function (on)
import Data.List (maximumBy, minimumBy)
import Linear.V2 (R1 (_x), R2 (_y), V2, crossZ)
import Linear.V3 (R3 (_z), V3 (..), cross)

-- TODO: Create partition before calling quickHull_
-- TODO: Handle collinear points (>=0, filter out p0)

_quickHull2 :: (Num a, Ord a) => [V2 a] -> V2 a -> V2 a -> [V2 a]
_quickHull2 points p0 p1
  | null onLeft = [p0]
  | otherwise = _quickHull2 onLeft p0 pm ++ _quickHull2 onLeft pm p1
 where
  onLeftDists = filter ((> 0) . snd) [(p, crossZ (p1 - p0) (p - p0)) | p <- points]
  onLeft = map fst onLeftDists
  pm = fst $ maximumBy (compare `on` snd) onLeftDists

-- Any shape with < 4 points is automatically its own convex hull, as it takes 3 points to make a 2D simplex
-- Pattern matching is probably faster than length...
quickHull2 :: (Num a, Ord a) => [V2 a] -> [V2 a]
quickHull2 [] = []
quickHull2 p@[_] = p
quickHull2 p@[_, _] = p
quickHull2 p@[_, _, _] = p
quickHull2 points =
  let maxXPoint = maximumBy (compare `on` (^. _x)) points
      minXPoint = minimumBy (compare `on` (^. _x)) points
      maxYPoint = maximumBy (compare `on` (^. _y)) points
      minYPoint = minimumBy (compare `on` (^. _y)) points
      topLeftHull = _quickHull2 points minXPoint maxYPoint
      topRightHull = _quickHull2 points maxYPoint maxXPoint
      bottomRightHull = _quickHull2 points maxXPoint minYPoint
      bottomLeftHull = _quickHull2 points minYPoint minXPoint
   in topLeftHull ++ topRightHull ++ bottomRightHull ++ bottomLeftHull

_quickHull2Par :: (Num a, Ord a) => [V2 a] -> V2 a -> V2 a -> [V2 a]
_quickHull2Par ps p0 p1
  | null onLeft = [p0]
  | otherwise = _quickHull2Par onLeft p0 pm ++ _quickHull2Par onLeft pm p1
 where
  onLeftDists = filter ((> 0) . snd) [(p, crossZ (p1 - p0) (p - p0)) | p <- ps]
  onLeft = map fst onLeftDists
  pm = fst $ maximumBy (compare `on` snd) onLeftDists

quickHull2Par :: (Num a, Ord a, NFData a) => [V2 a] -> [V2 a]
quickHull2Par [] = []
quickHull2Par p@[_] = p
quickHull2Par p@[_, _] = p
quickHull2Par p@[_, _, _] = p
quickHull2Par points =
  let maxXPoint = maximumBy (compare `on` (^. _x)) points
      minXPoint = minimumBy (compare `on` (^. _x)) points
      maxYPoint = maximumBy (compare `on` (^. _y)) points
      minYPoint = minimumBy (compare `on` (^. _y)) points
      (topLeftHull, topRightHull, bottomRightHull, bottomLeftHull) = runEval $ do
        topLeft <- rpar (force (_quickHull2Par points minXPoint maxYPoint))
        topRight <- rpar (force (_quickHull2Par points maxYPoint maxXPoint))
        bottomRight <- rpar (force (_quickHull2Par points maxXPoint minYPoint))
        bottomLeft <- rpar (force (_quickHull2Par points minYPoint minXPoint))
        _ <- rseq topLeft
        _ <- rseq topRight
        _ <- rseq bottomRight
        _ <- rseq bottomLeft
        return (topLeft, topRight, bottomLeft, bottomRight)
   in topLeftHull ++ topRightHull ++ bottomLeftHull ++ bottomRightHull

-- -- The cross product of (p1 - p0) and (p2 - p0) should be positive!
-- quickHull3_ :: (Num a, Ord a) => [V3 a] -> V3 a -> V3 a -> V3 a -> [V3 a]
-- quickHull3_ points p0 p1 p2 =
--   let norm = cross (p1 - p0) (p2 - p0)
--       distToPlane p = dot (p - p0) norm
--       pointsDists = [(p, distToPlane p) | p <- points]
--       onNormalSideDists = filter ((> 0) . snd) pointsDists
--       onNormalSide = map fst onNormalSideDists
--    in if length onNormalSideDists < 2
--         then p0 : onNormalSide
--         else
--           let pm = fst $ maximumBy (compare `on` snd) pointsDists
--            in quickHull3_ onNormalSide p0 p1 pm ++ quickHull3_ onNormalSide p1 p2 pm ++ quickHull3_ onNormalSide p2 p0 pm

-- -- TODO: Handle edge cases...
-- quickHull3 :: (Ord a) => [V3 a] -> [V3 a]
-- quickHull3 points =
--   let maxXPoint = maximumBy (compareVectorBy (^. _x)) points
--       minXPoint = minimumBy (compareVectorBy (^. _x)) points
--       maxYPoint = maximumBy (compareVectorBy (^. _y)) points
--       minYPoint = minimumBy (compareVectorBy (^. _y)) points
--       maxZPoint = maximumBy (compareVectorBy (^. _z)) points
--       minZPoint = minimumBy (compareVectorBy (^. _z)) points
--    in []
