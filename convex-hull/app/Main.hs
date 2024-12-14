module Main (main) where

import Linear.V2
import Linear.V3
import GrahamScan (grahamScan)
import QuickHull (quickHull2, quickHull2Par)
import QuickHull3D (quickHull3, quickHull3Par)
import Chans (chans2, chans2Par)
import System.Environment (getArgs)
import System.Random

randomV2s :: (RandomGen g, Random a, Fractional a) => g -> [V2 a]
randomV2s gen =
  let (x, gen') = randomR (0.1, 1) gen
      (y, gen'') = randomR (0.1, 1) gen'
   in V2 x y : randomV2s gen''

randomV3s :: (RandomGen g, Random a, Fractional a) => g -> [V3 a]
randomV3s gen =
  let (x, gen') = randomR (0, 1) gen
      (y, gen'') = randomR (0, 1) gen'
      (z, gen''') = randomR (0.1, 1) gen''
   in V3 x y z : randomV3s gen'''

main :: IO ()
main = do
  args <- getArgs
  case args of
    [numPointsString, algorithm, printFlag] -> do
      let n = read numPointsString :: Int
          points2d = take n $ randomV2s (mkStdGen 3) :: [V2 Double]
          points3d = take n $ randomV3s (mkStdGen 3) :: [V3 Double]
      case printFlag of
        "print" -> do
          putStrLn "Original points:"
          case algorithm of
            "quickHull3" -> mapM_ print points3d
            "quickHull3Par" -> mapM_ print points3d
            _ -> mapM_ print points2d
        "no-print" -> return ()
        _ -> putStrLn "Invalid print flag, choose: print or no-print."
      putStrLn "\nConvex hull:"
      case algorithm of
        "grahamScan" -> mapM_ print $ grahamScan points2d
        "quickHull2" -> mapM_ print $ quickHull2 points2d
        "quickHull2Par" -> mapM_ print $ quickHull2Par points2d
        "chans" -> mapM_ print $ chans2 n points2d
        "chansPar" ->  mapM_ print $ chans2Par n points2d
        "quickHull3" -> mapM_ print $ quickHull3 points3d
        "quickHull3Par" -> mapM_ print $ quickHull3Par points3d
        _ -> putStrLn "Invalid algorithm, choose: grahamScan, quickHull2, quickHullPar2, chans, chansPar, quickHull3, or quickHull3Par."
    _ -> putStrLn "usage: convex-hull <numPoints> <algorithm>"
