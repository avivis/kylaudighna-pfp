module Chans (jarvisMarch, chans2, chans2Par) where

import Control.Lens ((^.))
import Control.Parallel.Strategies (NFData, parMap, rdeepseq)
import Data.Function (on)
import Data.List (maximumBy, minimumBy)
import Data.List.Split (divvy)
import qualified Data.Vector as V
import Lib (orientation, sortPointsCCW)
import Linear.V2 (R1 (_x), V2)
import QuickHull (quickHull2)

jarvisMarch :: (Ord a, Floating a) => [V2 a] -> [V2 a]
jarvisMarch [] = []
jarvisMarch p@[_] = p
jarvisMarch p@[_, _] = p
jarvisMarch p@[_, _, _] = p
jarvisMarch points = _jarvisMarch start
 where
  start = minimum points
  _jarvisMarch p =
    let next = (minimumBy (orientation p) . filter (/= p)) points
     in if next == start then [p] else p : _jarvisMarch next

rightmostCCWPoint :: (Ord a, Floating a) => V2 a -> V.Vector (V2 a) -> V2 a
rightmostCCWPoint o ps = ps V.! binarySearch 0 (V.length ps - 1)
 where
  compareAdjacent i = (prev, next)
   where
    prev = orientation o (ps V.! i) (ps V.! ((i - 1) `mod` V.length ps))
    next = orientation o (ps V.! i) (ps V.! ((i + 1) `mod` V.length ps))

  binarySearch l r
    -- If we can't reach anymore points, we'll do l
    | l >= r = l
    -- If neither o->m->m-1 nor o->m->m+1 are clockwise turns, we are at the rightmost point!
    | mPrev /= LT && mNext /= LT = m
    -- If o->l->m is a clockwise turn and (o->l->l+1 is a clockwise turn or o->l->l-1 and o->l->l+1 turn the same way), or o->l->m is a counter-clockwise turn and o->m->m-1 is a clockwise turn, then search to the right of m (from l to m)
    | mSide == GT && (lNext == LT || lPrev == lNext) || mSide == LT && mPrev == LT = binarySearch l m
    -- Otherwise, search to the left of m
    | otherwise = binarySearch (m + 1) r
   where
    m = (l + r) `div` 2
    mSide = orientation o (ps V.! l) (ps V.! m) -- Which side is m on, relative to l?
    (lPrev, lNext) = compareAdjacent l -- What are the orientations of o->l->l-1 and o->l->l+1?
    (mPrev, mNext) = compareAdjacent m -- What are the orientations of o->m->m-1 and o->m->m+1?

-- in p : next : rightmostHullPoints

-- chans :: ([Point2D] -> [Point2D]) -> [Point2D] -> [Point2D]
-- chans _ [] = []
-- chans _ p@[_] = p
-- chans _ p@[_, _] = p
-- chans _ p@[_, _, _] = p
chans2 :: (Ord a, Floating a) => [V2 a] -> [V2 a]
chans2 ps = _chans2 [] start
 where
  m = length ps `div` 128 -- TODO: Make finding m work; this is just a value I found empirically
  subPoints = divvy m m ps
  subHulls = map (V.fromList . sortPointsCCW . quickHull2) subPoints -- sortPointsCCW . quickHull2 is actually faster...
  start = minimumBy (compare `on` (^. _x)) [V.minimumOn (^. _x) subHull | subHull <- subHulls] -- Point across all hulls with lowest X
  _chans2 h p =
    let next = maximumBy (orientation p) $ [(rightmostCCWPoint p . V.filter (/= p)) subHull | subHull <- subHulls]
     in if next == start then h else _chans2 (p : h) next

-- chans2 ps = _chans2 (1 :: Int)
--  where
--   _chans2 t
--     | expectedWrapTime > m = _chans2 (t + 1)
--     | otherwise = chansJarvisMarch subHulls
--    where
--     m = (2 :: Int) ^ (2 :: Int) ^ t
--     subPoints = divvy m m ps
--     subHulls = map (V.fromList . grahamScan) subPoints
--     l = sum $ map V.length subHulls -- Sum of lengths of all sub-hulls
--     expectedWrapTime = l * (floorFloatInt . logBase 2.0 . fromIntegral) l

chans2Par :: (RealFloat a, NFData a) => [V2 a] -> [V2 a]
chans2Par ps = _chans2Par [] start
 where
  m = length ps `div` 128 -- TODO: Make finding m work
  subPoints = divvy m m ps
  subHulls = parMap rdeepseq (V.fromList . sortPointsCCW . quickHull2) subPoints -- TODO: Play around with making this quickHull2Par, I found it was about the same speed
  start = minimumBy (compare `on` (^. _x)) [V.minimumOn (^. _x) subHull | subHull <- subHulls] -- Point across all hulls with lowest X
  _chans2Par h p =
   let next = maximumBy (orientation p) $ parMap rdeepseq (rightmostCCWPoint p . V.filter (/= p)) subHulls -- Eval all rightmost points in parallel
    in if next == start then h else _chans2Par (p : h) next

-- chans2Par ps = _chans2 (1 :: Int)
--  where
--   _chans2 t
--     | expectedWrapTime > m = _chans2 (t + 1)
--     | otherwise = jarvisMarch subPointsHulls
--    where
--     m = (2 :: Int) ^ (2 :: Int) ^ t
--     subPoints = divvy m m ps
--     subPointsHulls = concat (map grahamScan subPoints `using` parList rdeepseq)
--     l = length subPointsHulls
--     expectedWrapTime = l * (floorFloatInt . logBase 2.0 . fromIntegral) l
