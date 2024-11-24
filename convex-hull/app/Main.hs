module Main (main) where

main :: IO ()
main = print "Main"

-- import Lib
-- import QuickHull (quickHull)
-- import GrahamScan (grahamScan)
-- import System.Random
--
-- randomDouble :: Int -> [Double]
-- randomDouble seed = randoms (mkStdGen seed) :: [Double]
--
-- randomInt :: Int -> [Int]
-- randomInt seed = randoms (mkStdGen seed) :: [Int]
--
-- odds :: [a] -> [a]
-- odds [] = []
-- odds [x] = [x]
-- odds (x : _ : xs) = x : odds xs
--
-- evens :: [a] -> [a]
-- evens [] = []
-- evens [_] = []
-- evens (_ : x : xs) = x : evens xs
--
-- generateDoublePoints :: Int -> Int -> [Point2D]
-- generateDoublePoints seed n =
--     let randomVals = take (2 * n) $ randomDouble seed
--       in zipWith Point2D (odds randomVals) (evens randomVals)
--
-- generateIntPoints :: Int -> Int -> [Point2D]
-- generateIntPoints seed n =
--     let randomVals = take (2 * n) $ randomInt seed
--         scale x = fromIntegral (x `mod` 21) -- points between (0,0) and (20,20)
--       in zipWith Point2D (map scale $ odds randomVals) (map scale $ evens randomVals)
--
-- testConvexHullFn :: ([Point2D] -> [Point2D]) -> [Point2D] -> [Point2D]
-- testConvexHullFn f points = f points
--
-- main :: IO ()
-- main = do
--     let points = generateIntPoints 3 40
--     putStrLn "Original points:"
--     mapM_ print points
--
--     -- let hull = testConvexHullFn quickHull points
--     let hull = testConvexHullFn grahamScan points
--     putStrLn "\nHull points:"
--     mapM_ print hull
