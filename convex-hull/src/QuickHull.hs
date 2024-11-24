module QuickHull (quickHull2, quickHullPar2) where

import Control.DeepSeq (NFData, force)
import Control.Lens ((^.))
import Control.Parallel.Strategies (rpar, rseq, runEval)
import Data.Function (on)
import Data.List (maximumBy, minimumBy, partition)
import Linear.Metric (dot)
import Linear.V2 (R1 (_x), R2 (_y), V2 (V2), crossZ)
import Linear.V3 (V3 (..), cross)

-- TODO: Create partition before calling quickHull_
-- TODO: Handle collinear points (>=0, filter out p0)

compareVectorBy :: (Ord a) => (f a -> a) -> f a -> f a -> Ordering
compareVectorBy f x y = compare (f x) (f y)

-- The cross product of (p1 - p0) and (p2 - p0) should be positive!
quickHull3_ :: (Num a, Ord a) => [V3 a] -> V3 a -> V3 a -> V3 a -> [V3 a]
quickHull3_ points p0 p1 p2 =
  let norm = cross (p1 - p0) (p2 - p0)
      distToPlane p = dot (p - p0) norm
      pointsDists = [(p, distToPlane p) | p <- points]
      onNormalSideDists = filter ((> 0) . snd) pointsDists
      onNormalSide = map fst onNormalSideDists
   in if length onNormalSideDists < 2
        then p0 : onNormalSide
        else
          let pm = fst $ maximumBy (compare `on` snd) pointsDists
           in quickHull3_ onNormalSide p0 p1 pm ++ quickHull3_ onNormalSide p1 p2 pm ++ quickHull3_ onNormalSide p2 p0 pm

-- -- -- TODO: Handle edge cases...
-- quickHull3 :: (Ord a) => [V2 a] -> [V2 a]
-- quickHull3 points =
--   let

quickHull2_ :: (RealFloat a) => [V2 a] -> V2 a -> V2 a -> [V2 a]
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
quickHull2 :: (RealFloat a) => [V2 a] -> [V2 a]
quickHull2 [] = []
quickHull2 p@[_] = p
quickHull2 p@[_, _] = p
quickHull2 p@[_, _, _] = p
quickHull2 points =
  let maxXPoint = maximumBy (compareVectorBy (^. _x)) points
      minXPoint = minimumBy (compareVectorBy (^. _x)) points
   in quickHull2_ points minXPoint maxXPoint ++ quickHull2_ points maxXPoint minXPoint

-- Min X, min y, max X and max Y points all have to be part of the convex hull, so I do a 4-way
-- parallel approach here
quickHullPar2 :: (RealFloat a, NFData a) => [V2 a] -> [V2 a]
quickHullPar2 [] = []
quickHullPar2 p@[_] = p
quickHullPar2 p@[_, _] = p
quickHullPar2 p@[_, _, _] = p
quickHullPar2 points =
  let cmpX (V2 ax _) (V2 bx _) = compare ax bx
      cmpY (V2 _ ay) (V2 _ by) = compare ay by
      maxXPoint = maximumBy (compareVectorBy (^. _x)) points
      minXPoint = minimumBy (compareVectorBy (^. _x)) points
      maxYPoint = maximumBy (compareVectorBy (^. _y)) points
      minYPoint = minimumBy (compareVectorBy (^. _y)) points
      (topLeftHull, topRightHull, bottomRightHull, bottomLeftHull) = runEval $ do
        topLeft <- rpar (force (quickHull2_ points minXPoint maxYPoint))
        topRight <- rpar (force (quickHull2_ points maxYPoint maxXPoint))
        bottomRight <- rpar (force (quickHull2_ points maxXPoint minYPoint))
        bottomLeft <- rpar (force (quickHull2_ points minYPoint minXPoint))
        _ <- rseq topLeft
        _ <- rseq topRight
        _ <- rseq bottomRight
        _ <- rseq bottomLeft
        return (topLeft, topRight, bottomLeft, bottomRight)
   in topLeftHull ++ topRightHull ++ bottomLeftHull ++ bottomRightHull
