module Chans (chans, giftWrapping) where

import Data.List (minimumBy)

import Lib (Point2D (Point2D), crossProduct2D, squareDist2D)

giftWrapping :: [Point2D] -> [Point2D]
giftWrapping [] = []
giftWrapping p@[_] = p
giftWrapping p@[_, _] = p
giftWrapping p@[_, _, _] = p
giftWrapping points = giftWrapping_ start []
 where
  start = minimumBy (\(Point2D ax _) (Point2D bx _) -> compare ax bx) points
  giftWrapping_ curr hullPoints
    | next == start = hullPoints
    | otherwise = giftWrapping_ next (next : hullPoints)
   where
    next = leftmostPoint (tail points) (head points)
    leftmostPoint [] leftmost = leftmost
    leftmostPoint (check : rest) leftmost
      | check == curr = leftmostPoint rest leftmost
      | cross < 0 = leftmostPoint rest leftmost
      | cross > 0 = leftmostPoint rest check
      | otherwise = leftmostPoint rest (if squareDist2D curr check < squareDist2D curr leftmost then check else leftmost)
     where
      cross = crossProduct2D curr leftmost check

-- chans :: ([Point2D] -> [Point2D]) -> [Point2D] -> [Point2D]
-- chans _ [] = []
-- chans _ p@[_] = p
-- chans _ p@[_, _] = p
-- chans _ p@[_, _, _] = p
chans :: [Point2D] -> [Point2D]
chans _ = []
