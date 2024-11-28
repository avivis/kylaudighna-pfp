module GrahamScan (grahamScan) where

import Lib (orientation, sortPointsCW)
import Linear.V2 (V2)

grahamScan :: (Ord a, Num a) => [V2 a] -> [V2 a]
grahamScan [] = []
grahamScan p@[_] = p
grahamScan p@[_, _] = p
grahamScan p@[_, _, _] = p
grahamScan points = _grahamScan [] (sortPointsCW points)
 where
  _grahamScan hull [] = hull -- Base case: no more points, return hull
  _grahamScan [] (p : ps) = _grahamScan [p] ps -- Zero points on the hull, add the point
  _grahamScan hull@[_] (p : ps) = _grahamScan (p : hull) ps -- One point on the hull, add the point
  _grahamScan hull@(o1 : hullTail@(o0 : _)) ps@(p : psTail) =
    if orientation o0 o1 p /= GT -- If o0 -> o1 -> p is not a counter-clockwise turn...
      then p : _grahamScan (p : hull) psTail -- Then push p to the top of the hull!
      else _grahamScan hullTail ps -- Otherwise, pop from hull and try again
