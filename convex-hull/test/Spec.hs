-- module Main (main) where

-- import Chans (giftWrapping)
-- import GrahamScan (grahamScan)
-- import Linear.V2
-- import QuickHull (quickHull2, quickHullPar2, quickHullPar)
-- import System.IO (readFile)
-- import Data.List.Split (splitOn)

-- -- randomV2s :: (RandomGen g, Random a) => g -> [V2 a]
-- -- randomV2s gen =
-- --   let (x, gen') = random gen
-- --       (y, gen'') = random gen'
-- --    in V2 x y : randomV2s gen''

-- parseV2 :: String -> V2 Double
-- parseV2 line =
--   let [x, y] = map read (splitOn " " line)
--    in V2 x y

-- main :: IO ()
-- main = do
--   contents <- readFile "points-60000.txt"
--   let vecs = map parseV2 (lines contents)
--     --   grahamScanResults = grahamScan vecs
--     --   quickHullResults = quickHull2 vecs
--     --   quickHullParResults = quickHullPar2 vecs
--       quickHullPResults = quickHullPar vecs
-- --   print ""
-- --   print grahamScanResults
-- --   print quickHullResults
--   print quickHullPResults
-- --   print quickHullParResults

-- -- let points = take 65536 $ randomPoints 3
-- -- let points = take 1048576 $ randomPoints 3
-- -- print $ quickHull (take 2 points)
