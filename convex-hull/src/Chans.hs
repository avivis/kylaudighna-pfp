module Chans (jarvisMarch, chans2, chans2Par) where

import Control.Lens ((^.))
import Control.Parallel.Strategies (NFData, rdeepseq, parMap)
import Data.Function (on)
import Data.List (maximumBy, minimumBy)
import Data.List.Split (divvy)
import qualified Data.Vector as V
import GHC.Float.RealFracMethods (floorFloatInt)
import GrahamScan (grahamScan)
import Lib (comparePointsPolar, sortPointsCW)
import Linear.V2 (R1 (_x), V2 (V2))
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
    let next = (minimumBy (comparePointsPolar p) . filter (/= p)) points
     in if next == start then [p] else p : _jarvisMarch next

rightmostInCCWHull :: (Ord a, Floating a) => V2 a -> V.Vector (V2 a) -> V2 a
rightmostInCCWHull o h = h V.! binarySearch 0 (V.length h - 1)
 where
  comparePrevNext i = (prevCompare, nextCompare)
   where
    prevCompare = comparePointsPolar o (h V.! i) (h V.! ((i - 1) `mod` V.length h))
    nextCompare = comparePointsPolar o (h V.! i) (h V.! ((i + 1) `mod` V.length h))

  binarySearch l r
    -- If we can't reach anymore points, we'll do l
    | l >= r = l
    -- If neither o->m->m-1 nor o->m->m+1 are clockwise turns, we are at the rightmost point!
    | mPrevCompare /= LT && mNextCompare /= LT = m
    -- If o->l->m is a clockwise turn and (o->l->l+1 is a clockwise turn or o->l->l-1 and o->l->l+1 turn the same way), or o->l->m is a counter-clockwise turn and o->m->m-1 is a clockwise turn, then search to the right of m (from l to m)
    | mSide == GT && (lNextCompare == LT || lPrevCompare == lNextCompare) || mSide == LT && mPrevCompare == LT = binarySearch l m
    -- Otherwise, search to the left of m
    | otherwise = binarySearch (m + 1) r
   where
    m = (l + r) `div` 2
    mSide = comparePointsPolar o (h V.! l) (h V.! m) -- Which side is m on, relative to l?
    (lPrevCompare, lNextCompare) = comparePrevNext l -- What are the orientations of o->l->l-1 and o->l->l+1?
    (mPrevCompare, mNextCompare) = comparePrevNext m -- What are the orientations of o->m->m-1 and o->m->m+1?

chansJarvisMarch :: (Ord a, Floating a) => [V.Vector (V2 a)] -> [V2 a]
chansJarvisMarch subHulls = _chansJarvisMarch start []
 where
  start = minimumBy (compare `on` (^. _x)) [V.minimumOn (^. _x) subHull | subHull <- subHulls] -- Point across all hulls with lowest X
  _chansJarvisMarch p hull =
    let nextHull = p : hull
        rightmostHullPoints = [(rightmostInCCWHull p . V.filter (/= p)) subHull | subHull <- subHulls]
        next = maximumBy (comparePointsPolar p) rightmostHullPoints
     in if next == start then nextHull else _chansJarvisMarch next nextHull

chansJarvisMarchPar :: (NFData a, Ord a, Floating a) => [V.Vector (V2 a)] -> [V2 a]
chansJarvisMarchPar subHulls = _chansJarvisMarchPar start []
 where
  start = minimumBy (compare `on` (^. _x)) [V.minimumOn (^. _x) subHull | subHull <- subHulls] -- Point across all hulls with lowest X
  _chansJarvisMarchPar p hull =
    let nextHull = p : hull
        rightmostHullPoints = parMap rdeepseq (rightmostInCCWHull p . V.filter (/= p)) subHulls 
        next = maximumBy (comparePointsPolar p) rightmostHullPoints
     in if next == start then nextHull else _chansJarvisMarchPar next nextHull

-- in p : next : rightmostHullPoints

-- chans :: ([Point2D] -> [Point2D]) -> [Point2D] -> [Point2D]
-- chans _ [] = []
-- chans _ p@[_] = p
-- chans _ p@[_, _] = p
-- chans _ p@[_, _, _] = p
chans2 :: (Ord a, Floating a) => [V2 a] -> [V2 a]
chans2 ps =
  let
    m = length ps `div` 64 -- TODO: Make finding m work
    subPoints = divvy m m ps
    subHulls = map (V.fromList . grahamScan) subPoints
   in
    chansJarvisMarch subHulls

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
chans2Par ps =
  let
    m = length ps `div` 16 -- TODO: Make finding m work
    subPoints = divvy m m ps
    subHulls = parMap rdeepseq (V.fromList . grahamScan) subPoints
   in
    chansJarvisMarchPar subHulls
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
