module Main (main) where

--
-- import Lib
--
-- main :: IO ()
-- main = someFunc

import GrahamScan (grahamScan)
import Lib
import QuickHull (quickHull, quickHullPar)
import System.Random

randomDouble :: Int -> [Double]
randomDouble seed = randoms (mkStdGen seed) :: [Double]

randomPoints :: Int -> [Point2D]
randomPoints seed =
  let randomVals = randomDouble seed
   in zipWith Point2D (odds randomVals) (evens randomVals)

odds :: [a] -> [a]
odds [] = []
odds [x] = [x]
odds (x : _ : xs) = x : odds xs

evens :: [a] -> [a]
evens [] = []
evens [_] = []
evens (_ : x : xs) = x : evens xs

main :: IO ()
main = do
  let points = take 65536 $ randomPoints 3
  -- let points = take 1048576 $ randomPoints 3
  -- print $ quickHull (take 2 points)
  print $ grahamScan points
  print $ quickHull points
  print $ quickHullPar points
