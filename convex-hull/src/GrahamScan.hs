module GrahamScan ( grahamScan ) where

import Data.List (minimumBy, sortBy)

import Lib (Line2D (Line2D), Point2D (Point2D))

crossProduct :: Point2D -> Line2D -> Double
crossProduct (Point2D px py) (Line2D (Point2D x0 y0) (Point2D x1 y1)) = (x0 - px) * (y1 - py) - (y0 - py) * (x1 - px)

getMinYPoint :: [Point2D] -> Point2D
getMinYPoint points =
    let cmpY (Point2D _ ay) (Point2D _ by) = compare ay by
    in minimumBy cmpY points

angleToXAxis :: Point2D -> Point2D -> Double
angleToXAxis (Point2D x0 y0) (Point2D x y) = atan2 (y - y0) (x - x0)

sortPointsByAngle :: Point2D -> [Point2D] -> [Point2D]
sortPointsByAngle p0 points = sortBy (\p1 p2 -> compare (angleToXAxis p0 p1) (angleToXAxis p0 p2)) points

isLeftTurn :: Point2D -> Point2D -> Point2D -> Bool
isLeftTurn p1 p2 p3 = crossProduct p2 (Line2D p1 p3) >= 0

grahamScan :: [Point2D] -> [Point2D]
grahamScan points =
    let p0 = getMinYPoint points
        sortedPoints = sortPointsByAngle p0 (filter (/= p0) points)
        convexHull = buildHull [p0] sortedPoints
    in convexHull
    
buildHull :: [Point2D] -> [Point2D] ->[Point2D]
buildHull hull [] = hull -- base case: no more points
buildHull (p1:p2:hull) (p3:points)
  | isLeftTurn p1 p2 p3 = buildHull (p3:p1:p2:hull) points  -- left turn or collinear: add p3 to the hull and continue
  | otherwise           = buildHull (p2:hull) (p3:points)  -- right turn: pop p1 from hull and continue
buildHull hull (p3:points) = buildHull (p3:hull) points  -- if there are less than 2 points, just push p3 onto hull and continue