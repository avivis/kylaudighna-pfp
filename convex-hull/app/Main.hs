module Main (main) where

import Linear.V2
import GrahamScan (grahamScan)
import QuickHull (quickHull2, quickHull2Par)
import Chans (chans2, chans2Par)
import System.Environment (getArgs)
import System.Random

randomV2s :: (RandomGen g, Random a) => g -> [V2 a]
randomV2s gen =
  let (x, gen') = random gen
      (y, gen'') = random gen'
   in V2 x y : randomV2s gen''


main :: IO ()
main = do
  args <- getArgs
  case args of
    [numPointsString, algorithm] -> do
      let n = read numPointsString :: Int
          points = take n $ randomV2s (mkStdGen 3) :: [V2 Double]
      case algorithm of
        "grahamScan" -> print $ grahamScan points
        "quickHull" -> print $ quickHull2 points
        "quickHullPar" -> print $ quickHull2Par points
        "chans" -> print $ chans2 n points
        "chansPar" ->  print $ chans2Par n points
        _ -> putStrLn "Invalid algorithm, choose: grahamScan, quickHull, quickHullPar, chans, or chansPar."
    _ -> putStrLn "usage: convex-hull <numPoints> <algorithm>"
