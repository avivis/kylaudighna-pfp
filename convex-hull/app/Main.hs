module Main (main) where

import Chans (chans2, chans2Par)
import GrahamScan (grahamScan)
import Linear.V2 (V2 (V2))

-- import Linear.V3 (V3 (V3))

import Lib (isCCWTurn, sortPointsCCW)
import QuickHull (quickHull2, quickHull2Par)
import System.Random

randomV2s :: (RandomGen g, Random a) => g -> [V2 a]
randomV2s gen =
  let (x, gen') = random gen
      (y, gen'') = random gen'
   in V2 x y : randomV2s gen''

-- randomV3s :: (RandomGen g, Random a) => g -> [V3 a]
-- randomV3s gen =
--   let (x, gen') = random gen
--       (y, gen'') = random gen'
--       (z, gen''') = random gen''
--    in V3 x y z : randomV3s gen'''

main :: IO ()
main = do
  -- let points = take 8 $ map (*100) $ randomV2s (mkStdGen 3) :: [V2 Double]
  -- let points = take 16 $ map (*100) $ randomV2s (mkStdGen 3) :: [V2 Double]
  -- let points = take 65536 $ randomV2s (mkStdGen 3) :: [V2 Double]
  -- let points = take 1048576 $ randomV2s (mkStdGen 3) :: [V2 Double]
  let points = take 2097152 $ randomV2s (mkStdGen 3) :: [V2 Double]
  -- assert (verifyConvexHull2Algorithm points grahamScan) return ()
  --
  -- assert (verifyConvexHull2Algorithm points quickHull2) return ()
  -- print $ sortPointsCCW points
  -- print $ grahamScan points
  -- print $ quickHull2 points
  -- print $ quickHull2Par points
  -- print $ chans2 points
  print $ chans2Par points
  --
  -- assert (verifyConvexHull2Algorithm points chans2) return ()
  -- assert (verifyConvexHull2Algorithm points chans2Par) return ()
