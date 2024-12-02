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

jarvisMarch :: (Ord a, Num a) => [V2 a] -> [V2 a]
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

rightmostCCWPoint :: (Ord a, Num a) => V2 a -> V.Vector (V2 a) -> V2 a
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

-- chans :: ([Point2D] -> [Point2D]) -> [Point2D] -> [Point2D]
-- chans _ [] = []
-- chans _ p@[_] = p
-- chans _ p@[_, _] = p
-- chans _ p@[_, _, _] = p
-- chans2 :: (Ord a, Floating a) => [V2 a] -> [V2 a]
-- chans2 ps = _chans2 (100 :: Int)
--  where
--   start = minimumBy (compare `on` (^. _x)) ps -- Point across all hulls with lowest X
--   _chans2 t = chansJarvisMarch [] start 0
--    where
--     m = min (length ps) ((2 :: Int) ^ ((2 :: Int) ^ t))
--     subPoints = divvy m m ps
--     subHulls = map (V.fromList . sortPointsCCW . quickHull2) subPoints
--     chansJarvisMarch hull p h
--       -- | h > m = _chans2 (t + 1)
--       | next == start = hull
--       | otherwise = chansJarvisMarch (p : hull) next (h + 1)
--      where
--       next = maximumBy (orientation p) $ [(rightmostCCWPoint p . V.filter (/= p)) subHull | subHull <- subHulls]

chans2 :: (Ord a, Num a) => [V2 a] -> [V2 a]
chans2 ps = _chans2 start
 where
  m = length ps `div` 128 -- TODO: Make finding m better; this is just a value I found empirically
  subPoints = divvy m m ps
  subHulls = map (V.fromList . sortPointsCCW . quickHull2) subPoints
  start = minimumBy (compare `on` (^. _x)) [V.minimumOn (^. _x) subHull | subHull <- subHulls] -- Point across all hulls with lowest X
  _chans2 p =
   let next = maximumBy (orientation p) $ map (rightmostCCWPoint p . V.filter (/= p)) subHulls -- Eval all rightmost points in parallel
    in if next == start then [p] else p : _chans2 next

chans2Par :: (Ord a, Num a, NFData a) => [V2 a] -> [V2 a]
chans2Par ps = _chans2Par start
 where
  m = length ps `div` 128 -- TODO: Make finding m work; this is just a value I found empirically
  subPoints = divvy m m ps
  subHulls = parMap rdeepseq (V.fromList . sortPointsCCW . quickHull2) subPoints -- TODO: Play around with making this quickHull2Par, I found it was about the same speed
  start = minimumBy (compare `on` (^. _x)) [V.minimumOn (^. _x) subHull | subHull <- subHulls] -- Point across all hulls with lowest X
  _chans2Par p =
   let next = maximumBy (orientation p) $ parMap rdeepseq (rightmostCCWPoint p . V.filter (/= p)) subHulls -- Eval all rightmost points in parallel
    in if next == start then [p] else p : _chans2Par next
