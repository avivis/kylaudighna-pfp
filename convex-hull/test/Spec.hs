import Lib
import QuickHull (quickHull)
import System.Random

randomDouble :: Integer -> [Double]
randomDouble seed = randoms (mkStdGen seed) :: [Double]

odds :: [a] -> [a]
odds [] = []
odds [x] = [x]
odds (x : _ : xs) = x : odds xs

evens :: [a] -> [a]
evens [] = []
evens [_] = []
evens (_ : x : xs) = x : evens xs

testConvexHullFn :: ([Point2D] -> [Point2D]) -> Integer -> Integer -> [Point2D]
testConvexHullFn f seed n =
  let randomVals = take (2 * n) $ randomDouble seed
      points = zip (odds randomVals) (evens randomVals)
   in f points

main :: IO ()
main = do
  let pointNum = 65536
      randomVals = take (2 * pointNum) randomDouble
      points = zip (odds randomVals) (evens randomVals)
   in print $ quickHull points
