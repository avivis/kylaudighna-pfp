module Chans (jarvisMarch, chans2, chans2Par) where

import Control.Parallel.Strategies (NFData, parList, rdeepseq, using)
import Data.List (minimumBy)
import Data.List.Split (divvy)
import qualified Data.Vector as V
import GHC.Float.RealFracMethods (floorFloatInt)
import GrahamScan (grahamScan)
import Lib (comparePointsPolar, sortPointsCW)
import Linear.V2 (V2 (V2))
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
    prevCompare = comparePointsPolar o (h V.! ((i - 1) `mod` V.length h)) (h V.! i)  
    nextCompare = comparePointsPolar o (h V.! i) (h V.! ((i + 1) `mod` V.length h))
  binarySearch l r
    -- If we can't reach anymore points, we'll do l
    | l >= r = l
    -- If o->m-1->m is not a counter-clockwise turn, and o->m->m+1 is a counter-clockwise turn, we are at the rightmost point!
    | mPrevCompare /= GT && mNextCompare == GT = m
    -- If o->l->m is a counter-clockwise turn and (o->l-1->l is not a counter-clockwise turn or ), or if o->l->m is a clockwise turn and o->m->m+1 is not a clockwise turn, then it could be between l and m
    | (mSide == GT && (lPrevCompare /= GT || lPrevCompare /= lNextCompare)) || (mSide == LT && mNextCompare /= LT) = binarySearch l m
    -- -- If o -> m-1 -> m turns CCW or is colinear, then the rightmost point has to come after
    | otherwise = binarySearch (m + 1) r
   where
    m = (l + r) `div` 2
    mSide = comparePointsPolar o (h V.! l) (h V.! m) -- Which side is m on, relative to l?
    (lPrevCompare, lNextCompare) = comparePrevNext l
    (mPrevCompare, mNextCompare) = comparePrevNext m

chansJarvisMarch :: (Ord a, Floating a) => [V.Vector (V2 a)] -> [V2 a]
chansJarvisMarch subHulls = _chansJarvisMarch start []
 where
  start = minimum [V.minimum subHull | subHull <- subHulls] -- Point across all hulls with lowest X
  _chansJarvisMarch p hull =
    let nextHull = p : hull
        next = minimumBy (comparePointsPolar p) [(rightmostInCCWHull p . V.filter (/= p)) subHull | subHull <- subHulls]
     in if next == start then nextHull else _chansJarvisMarch next nextHull

-- chans :: ([Point2D] -> [Point2D]) -> [Point2D] -> [Point2D]
-- chans _ [] = []
-- chans _ p@[_] = p
-- chans _ p@[_, _] = p
-- chans _ p@[_, _, _] = p
chans2 :: (Ord a, Floating a) => [V2 a] -> [V2 a]
chans2 ps = _chans2 (1 :: Int)
 where
  _chans2 t
    | expectedWrapTime > m = _chans2 (t + 1)
    | otherwise = chansJarvisMarch subHulls
   where
    m = (2 :: Int) ^ (2 :: Int) ^ t
    subPoints = divvy m m ps
    subHulls = map (V.fromList . grahamScan) subPoints
    l = sum $ map V.length subHulls -- Sum of lengths of all sub-hulls
    expectedWrapTime = l * (floorFloatInt . logBase 2.0 . fromIntegral) l

chans2Par :: (RealFloat a, NFData a) => [V2 a] -> [V2 a]
chans2Par ps = _chans2 (1 :: Int)
 where
  _chans2 t
    | expectedWrapTime > m = _chans2 (t + 1)
    | otherwise = jarvisMarch subPointsHulls
   where
    m = (2 :: Int) ^ (2 :: Int) ^ t
    subPoints = divvy m m ps
    subPointsHulls = concat (map grahamScan subPoints `using` parList rdeepseq)
    l = length subPointsHulls
    expectedWrapTime = l * (floorFloatInt . logBase 2.0 . fromIntegral) l
