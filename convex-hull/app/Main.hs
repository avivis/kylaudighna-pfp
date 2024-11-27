module Main (main) where

import GrahamScan (grahamScan)
import Linear.V2
import QuickHull (quickHull2, quickHull2Par)
import System.Random

randomV2s :: (RandomGen g, Random a) => g -> [V2 a]
randomV2s gen =
  let (x, gen') = random gen
      (y, gen'') = random gen'
   in V2 x y : randomV2s gen''


main :: IO ()
main = do
  let vecs = take 1000000 $ randomV2s (mkStdGen 3) :: [V2 Double]
      -- grahamScanResults = grahamScan vecs
      -- quickHullResults = quickHull2 vecs
      quickHullParResults = quickHull2Par vecs
--   print ""
  -- print grahamScanResults
  -- print quickHullResults
  print quickHullParResults
