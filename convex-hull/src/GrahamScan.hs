module GrahamScan (grahamScan) where

import Lib (sortPointsCCW, isCCWTurn)
import Linear.V2 (V2)

_grahamScan :: (Ord a, Floating a) => [V2 a] -> [V2 a] -> [V2 a]
_grahamScan hull [] = hull -- base case: no more points
_grahamScan (o1 : o0 : hull) (p : ps) =
  if isCCWTurn o0 o1 p
    then _grahamScan (p : o1 : o0 : hull) ps -- crossZ >= 0 means left turn or colinear
    else _grahamScan (o0 : hull) (p : ps) -- right turn: pop p1 from hull and continue
_grahamScan h (p : ps) = _grahamScan (p : h) ps -- if there are less than 2 points, just push p onto hull and continue

grahamScan :: (Ord a, Floating a) => [V2 a] -> [V2 a]
grahamScan [] = []
grahamScan p@[_] = p
grahamScan p@[_, _] = p
grahamScan p@[_, _, _] = p
grahamScan points = _grahamScan [] (sortPointsCCW points)
