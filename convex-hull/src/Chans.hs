module Chans (chans2, chans2Par) where

import Control.Parallel.Strategies
import Data.List (minimumBy)
import Data.List.Split (divvy)
import qualified Data.Vector as V
import GHC.Float.RealFracMethods (floorFloatInt)
import GrahamScan (grahamScan)
import Lib (comparePointsPolar)
import Linear.V2 (V2)
import QuickHull (quickHull2)

jarvisMarch2 :: (Ord a, Floating a) => [V2 a] -> [V2 a]
jarvisMarch2 [] = []
jarvisMarch2 p@[_] = p
jarvisMarch2 p@[_, _] = p
jarvisMarch2 p@[_, _, _] = p
jarvisMarch2 points =
  let start = minimum points
      _jarvisMarch p =
        let next = (minimumBy (comparePointsPolar p) . filter (/= p)) points
         in if next == start then [p] else p : _jarvisMarch next
   in _jarvisMarch start

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
    | otherwise = (quickHull2 . concatMap V.toList) subPointsHulls
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
    | otherwise = jarvisMarch2 subPointsHulls
   where
    m = (2 :: Int) ^ (2 :: Int) ^ t
    subPoints = divvy m m ps
    subPointsHulls = concat $ parMap rdeepseq quickHull2 subPoints
    l = length subPointsHulls
    expectedWrapTime = l * (floorFloatInt . logBase 2.0 . fromIntegral) l
