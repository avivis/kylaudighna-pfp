module GrahamScan (grahamScan) where

import Lib (comparePointsPolar, sortPointsCW)
import Linear.V2 (V2)

grahamScan :: (Ord a, Floating a) => [V2 a] -> [V2 a]
grahamScan [] = []
grahamScan p@[_] = p
grahamScan p@[_, _] = p
grahamScan p@[_, _, _] = p
grahamScan points = _grahamScan [] (sortPointsCW points)
 where
  _grahamScan :: (Ord a, Floating a) => [V2 a] -> [V2 a] -> [V2 a]
  _grahamScan hull [] = hull -- Base case: no more points, return hull
  _grahamScan [] (p : ps) = _grahamScan [p] ps -- Zero points on the hull, add the point
  _grahamScan hull@[_] (p : ps) = _grahamScan (p : hull) ps -- One point on the hull, add the point
  _grahamScan hull@(o1 : hullTail@(o0 : _)) ps@(p : psTail) =
    if comparePointsPolar o0 o1 p /= GT -- If o0 -> o1 -> p is a CCW turn...
      then _grahamScan (p : hull) psTail -- Then push p to the top of the hull!
      else _grahamScan hullTail ps -- Otherwise, pop from hull and try again
