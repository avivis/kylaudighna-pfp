module Main (main) where

import Linear.V2 (V2(..))
import Linear.V3 (V3(..))
import GrahamScan (grahamScan)
import QuickHull (quickHull2, quickHull2Par)
import QuickHull3D (quickHull3, quickHull3Par)
import Chans (chans2, chans2Par)
import System.Environment (getArgs)
import System.Random

randomV2s :: (RandomGen g, Random a, Fractional a) => g -> [V2 a]
randomV2s gen =
  let (x, gen') = randomR (0, 1) gen
      (y, gen'') = randomR (0, 1) gen'
   in V2 x y : randomV2s gen''

randomV3s :: (RandomGen g, Random a, Fractional a) => g -> [V3 a]
randomV3s gen =
  let (x, gen') = randomR (0, 1) gen
      (y, gen'') = randomR (0, 1) gen'
      (z, gen''') = randomR (0.1, 1) gen''
   in V3 x y z : randomV3s gen'''

printPoints :: Show a => [a] -> IO ()
printPoints points = do
    putStrLn "generated points:"
    mapM_ (putStrLn . show) points
    putStrLn $ "total points: " ++ show (length points)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [numPointsString, algorithm, dimension] -> do
      let numPoints = read numPointsString :: Int
      putStrLn $ "generating " ++ show numPoints ++ " points..."
      case dimension of
        "2d" -> do
          let points = take numPoints $ randomV2s (mkStdGen 7) :: [V2 Double]
          putStrLn "running 2D algorithm..."
          case algorithm of
            "grahamScan" -> print $ grahamScan points
            "quickHull" -> print $ quickHull2 points
            "quickHullPar" -> print $ quickHull2Par points
            "chans" -> print $ chans2 points
            "chansPar" -> print $ chans2Par numPoints points
            _ -> putStrLn "invalid algorithm"
        "3d" -> do
          let points = take numPoints $ randomV3s (mkStdGen 7) :: [V3 Double]
          printPoints points
          putStrLn "running 3D algorithm..."
          case algorithm of
            "quickHull" -> do
                putStrLn "Starting QuickHull 3D..."
                let result = quickHull3 points
                putStrLn $ "Found " ++ show (length result) ++ " vertices in convex hull"
                print result
            "quickHullPar" -> do
                putStrLn "Starting Parallel QuickHull 3D..."
                let result = quickHull3Par points
                putStrLn $ "Found " ++ show (length result) ++ " vertices in convex hull"
                print result
            _ -> putStrLn "invalid algorithm"
        _ -> putStrLn "choose: 2d or 3d"
    _ -> putStrLn "usage: convex-hull <numPoints> <algorithm> <dimension>"