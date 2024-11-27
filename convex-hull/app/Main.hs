module Main (main) where

import GrahamScan (grahamScan)
import Linear.V2
import QuickHull (quickHull2, quickHull2Par, quickHull2Par2way)
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
      let numPoints = read numPointsString :: Int
          vecs = take numPoints $ randomV2s (mkStdGen 3) :: [V2 Double]
      case algorithm of
        "grahamScan" -> print $ grahamScan vecs
        "quickHull" -> print $ quickHull2 vecs
        "quickHullPar" -> print $ quickHull2Par vecs
        "quickHullPar2way" -> print $ quickHull2Par2way vecs
        _ -> putStrLn "Invalid algorithm, choose: grahamScan, quickHull, or quickHullPar."
    _ -> putStrLn "usage: convex-hull <numPoints> <algorithm>"
