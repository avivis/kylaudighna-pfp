module QuickHull (quickHull, quickHullPar) where

import Control.DeepSeq (force)
import Control.Parallel.Strategies (rpar, rseq, runEval)
import Data.List (maximumBy, minimumBy)
import Lib (Point2D (Point2D))

crossProduct :: Point2D -> Point2D -> Point2D -> Double
crossProduct (Point2D x0 y0) (Point2D x1 y1) (Point2D x2 y2) = (x0 - x2) * (y1 - y2) - (y0 - y2) * (x1 - x2)

-- TODO: Make a dimension-agnostic QuickHull

-- TODO: Create partition before calling quickHull_
-- TODO: Handle collinear points (>=0, filter out p0)
quickHull_ :: [Point2D] -> Point2D -> Point2D -> [Point2D]
quickHull_ [] _ _ = []
quickHull_ p@[_] _ _ = p
quickHull_ p@[_, _] _ _ = p
quickHull_ p@[_, _, _] _ _ = p
quickHull_ points p0 p1 =
  let pointsDists = [(p, crossProduct p0 p1 p) | p <- points]
      leftOfLineDists = filter ((> 0) . snd) pointsDists
      leftOfLine = map fst leftOfLineDists
   in -- (notColinearDists, colinearDists) = partition ((== 0) . snd) rightOfLineDists
      if length leftOfLineDists < 2
        then p0 : leftOfLine
        else
          let maxPoint = (fst . maximumBy (\(_, x) (_, y) -> compare x y)) leftOfLineDists
           in quickHull_ leftOfLine p0 maxPoint ++ quickHull_ leftOfLine maxPoint p1

-- Any shape with < 4 points is automatically its own convex hull, as it takes 3 points to make a 2D simplex
quickHull :: [Point2D] -> [Point2D]
quickHull [] = []
quickHull p@[_] = p
quickHull p@[_, _] = p
quickHull p@[_, _, _] = p
quickHull points =
  let cmpX (Point2D ax _) (Point2D bx _) = compare ax bx
      maxXPoint = maximumBy cmpX points
      minXPoint = minimumBy cmpX points
   in quickHull_ points minXPoint maxXPoint ++ quickHull_ points maxXPoint minXPoint

-- Min X, min y, max X and max Y points all have to be part of the convex hull, so I do a 4-way
-- parallel approach here
quickHullPar :: [Point2D] -> [Point2D]
quickHullPar [] = []
quickHullPar p@[_] = p
quickHullPar p@[_, _] = p
quickHullPar p@[_, _, _] = p
quickHullPar points =
  let cmpX (Point2D ax _) (Point2D bx _) = compare ax bx
      cmpY (Point2D _ ay) (Point2D _ by) = compare ay by
      maxXPoint = maximumBy cmpX points
      maxYPoint = maximumBy cmpY points
      minXPoint = minimumBy cmpX points
      minYPoint = minimumBy cmpY points
      (topLeftHull, topRightHull, bottomRightHull, bottomLeftHull) = runEval $ do
        topLeft <- rpar (force (quickHull_ points minXPoint maxYPoint))
        topRight <- rpar (force (quickHull_ points maxYPoint maxXPoint))
        bottomRight <- rpar (force (quickHull_ points maxXPoint minYPoint))
        bottomLeft <- rpar (force (quickHull_ points minYPoint minXPoint))
        _ <- rseq topLeft
        _ <- rseq topRight
        _ <- rseq bottomRight
        _ <- rseq bottomLeft
        return (topLeft, topRight, bottomLeft, bottomRight)
   in topLeftHull ++ topRightHull ++ bottomLeftHull ++ bottomRightHull
