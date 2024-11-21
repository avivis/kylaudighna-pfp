module QuickHull (quickHull) where

import Data.List (maximumBy, minimumBy)

import Lib (Line2D (Line2D), Point2D (Point2D))

crossProduct :: Point2D -> Line2D -> Double
crossProduct (Point2D px py) (Line2D (Point2D x0 y0) (Point2D x1 y1)) = (x0 - px) * (y1 - py) - (y0 - py) * (x1 - px)

-- TODO: Make a dimension-agnostic QuickHull

-- TODO: Create partition before calling quickHull_
-- TODO: Handle collinear points (>=0, filter out p0) 
quickHull_ :: [Point2D] -> Line2D -> [Point2D]
quickHull_ points l@(Line2D p0 p1) =
  let pointsDists = [(p, crossProduct p l) | p <- points]
      rightOfLineDists = filter ((> 0) . snd) pointsDists
      rightOfLine = map fst rightOfLineDists
   in if length rightOfLineDists < 2
        then p0 : rightOfLine
        else
          let maxPoint = (fst . maximumBy (\(_, x) (_, y) -> compare x y)) rightOfLineDists
           in quickHull_ rightOfLine (Line2D p0 maxPoint) ++ quickHull_ rightOfLine (Line2D maxPoint p1)

quickHull :: [Point2D] -> [Point2D]
quickHull [p] = [p] -- Stupid edge case with one point...
quickHull points =
  let cmpX (Point2D ax _) (Point2D bx _) = compare ax bx
      maxXPoint = maximumBy cmpX points
      minXPoint = minimumBy cmpX points
   in quickHull_ points (Line2D minXPoint maxXPoint) ++ quickHull_ points (Line2D maxXPoint minXPoint)
