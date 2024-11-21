module Main (main) where

--
-- import Lib
--
-- main :: IO ()
-- main = someFunc

import Lib
import QuickHull (quickHull)
import System.Random

randomDouble :: Int -> [Double]
randomDouble seed = randoms (mkStdGen seed) :: [Double]

odds :: [a] -> [a]
odds [] = []
odds [x] = [x]
odds (x : _ : xs) = x : odds xs

evens :: [a] -> [a]
evens [] = []
evens [_] = []
evens (_ : x : xs) = x : evens xs

testConvexHullFn :: ([Point2D] -> [Point2D]) -> Int -> Int -> [Point2D]
testConvexHullFn f seed n =
  let randomVals = take (2 * n) $ randomDouble seed
      points = zipWith Point2D (odds randomVals) (evens randomVals)
   in f points

main :: IO ()
main = do
  print $ testConvexHullFn quickHull 3 65536
