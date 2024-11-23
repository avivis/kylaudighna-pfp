module GrahamScan (grahamScan) where

import Data.List (minimumBy, sortBy)

import Lib (Point2D (Point2D))

-- crossProduct :: Point2D -> Line2D -> Double
-- crossProduct (Point2D px py) (Line2D (Point2D x0 y0) (Point2D x1 y1)) = (x0 - px) * (y1 - py) - (y0 - py) * (x1 - px)

crossProduct :: Point2D -> Point2D -> Point2D -> Double
crossProduct (Point2D x0 y0) (Point2D x1 y1) (Point2D x2 y2) = (x0 - x2) * (y1 - y2) - (y0 - y2) * (x1 - x2)

angleToXAxis :: Point2D -> Point2D -> Double
angleToXAxis (Point2D x0 y0) (Point2D x y) = atan2 (y - y0) (x - x0)

sortPointsByAngle :: Point2D -> [Point2D] -> [Point2D]
sortPointsByAngle p0 = sortBy (\p1 p2 -> compare (angleToXAxis p0 p1) (angleToXAxis p0 p2))

isLeftTurn :: Point2D -> Point2D -> Point2D -> Bool
isLeftTurn p0 p1 p2 = crossProduct p0 p1 p2 >= 0

buildHull :: [Point2D] -> [Point2D] -> [Point2D]
buildHull hull [] = hull -- base case: no more points
buildHull (p1 : p0 : hull) (p : points)
  | isLeftTurn p0 p1 p = buildHull (p : p1 : p0 : hull) points -- left turn or collinear: add p to the hull and continue
  | otherwise = buildHull (p0 : hull) (p : points) -- right turn: pop p1 from hull and continue
buildHull hull (p : points) = buildHull (p : hull) points -- if there are less than 2 points, just push p onto hull and continue

grahamScan :: [Point2D] -> [Point2D]
grahamScan points =
  let pYMin = minimumBy (\(Point2D _ ay) (Point2D _ by) -> compare ay by) points
      -- sortedPoints = sortPointsByAngle pYMin (filter (/= pYMin) points)
      sortedPoints = sortPointsByAngle pYMin points
      convexHull = buildHull [] sortedPoints
   in convexHull
