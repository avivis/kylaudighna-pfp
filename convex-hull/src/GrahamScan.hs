module GrahamScan (grahamScan) where

import Data.List (minimumBy, sortOn)
import Linear.V2

-- import Lib (Point2D (Point2D), crossProduct2D)

angleToXAxis :: (RealFloat a) => V2 a -> V2 a -> a
angleToXAxis (V2 x0 y0) (V2 x y) = atan2 (y - y0) (x - x0)

sortPointsByAngle :: (RealFloat a) => V2 a -> ([V2 a] -> [V2 a])
sortPointsByAngle p0 = sortOn (angleToXAxis p0)

isLeftTurn :: (RealFloat a) => V2 a -> V2 a -> V2 a -> Bool
isLeftTurn p0 p1 p2 = crossZ (p1 - p0) (p2 - p0) >= 0

buildHull :: (RealFloat a) => [V2 a] -> [V2 a] -> [V2 a]
buildHull hull [] = hull -- base case: no more points
buildHull (p1 : p0 : hull) (p : points)
  | isLeftTurn p0 p1 p = buildHull (p : p1 : p0 : hull) points -- left turn or collinear: add p to the hull and continue
  | otherwise = buildHull (p0 : hull) (p : points) -- right turn: pop p1 from hull and continue
buildHull hull (p : points) = buildHull (p : hull) points -- if there are less than 2 points, just push p onto hull and continue

grahamScan :: (RealFloat a) => [V2 a] -> [V2 a]
grahamScan [] = []
grahamScan p@[_] = p
grahamScan p@[_, _] = p
grahamScan p@[_, _, _] = p
grahamScan points =
  let pYMin = minimumBy (\(V2 _ ay) (V2 _ by) -> compare ay by) points
      -- sortedPoints = sortPointsByAngle pYMin (filter (/= pYMin) points)
      sortedPoints = sortPointsByAngle pYMin points
      convexHull = buildHull [] sortedPoints
   in convexHull
