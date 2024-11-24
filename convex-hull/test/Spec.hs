module Main (main) where

--
-- import Lib
--
-- main :: IO ()
-- main = someFunc

import Chans (giftWrapping)
import GrahamScan (grahamScan)
import Linear.V2
import QuickHull (quickHull2, quickHullPar2)
import System.Random

randomV2s :: (RandomGen g, Random a) => g -> [V2 a]
randomV2s gen =
  let (x, gen') = random gen
      (y, gen'') = random gen'
   in V2 x y : randomV2s gen''

main :: IO ()
main = do
  let vecs = take 1048576 $ randomV2s (mkStdGen 3) :: [V2 Double]
      grahamScanResults = grahamScan vecs
      quickHullResults = quickHull2 vecs
      quickHullParResults = quickHullPar2 vecs
  print ""
  print grahamScanResults
  print quickHullResults
  print quickHullParResults

-- let points = take 65536 $ randomPoints 3
-- let points = take 1048576 $ randomPoints 3
-- print $ quickHull (take 2 points)
