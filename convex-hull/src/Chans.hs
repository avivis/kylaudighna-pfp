module Chans (jarvisMarch, chans2, chans2Par) where

import Control.Parallel.Strategies (NFData, parList, rdeepseq, using)
import Data.List (minimumBy)
import Data.List.Split (divvy)
import qualified Data.Vector as V
import GHC.Float.RealFracMethods (floorFloatInt)
import GrahamScan (grahamScan)
import Lib (comparePointsPolar)
import Linear.V2 (V2)
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
rightmostInCCWHull p ccwHull = ccwHull V.! binarySearch 0 (V.length ccwHull - 1)
 where
  binarySearch l r =
    if l == r
      then l
      else
        let m = (l + r) `div` 2
            v0 = ccwHull V.! m
            v1 = ccwHull V.! ((m + 1) `mod` V.length ccwHull)
         in if comparePointsPolar p v0 v1 == GT then binarySearch (m + 1) r else binarySearch l m

chansJarvisMarch :: (Ord a, Floating a) => [V.Vector (V2 a)] -> [V2 a]
chansJarvisMarch subHulls = _chansJarvisMarch start []
 where
  start = minimum [V.minimum hull | hull <- subHulls] -- Point across all hulls with lowest X
  _chansJarvisMarch p hull =
    let next = minimumBy (comparePointsPolar p) [(rightmostInCCWHull p . V.filter (/= p)) subHull | subHull <- subHulls]
     in if next == start then hull else _chansJarvisMarch next (p : hull)

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
    | otherwise = chansJarvisMarch subPointsHulls
   where
    m = (2 :: Int) ^ (2 :: Int) ^ t
    subPoints = divvy m m ps
    subPointsHulls = map (V.fromList . grahamScan) subPoints
    l = sum $ map V.length subPointsHulls
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
