{-# LANGUAGE BangPatterns #-}

import Lib
import QuickHull (quickHull)
import ParallelQuickHull (quickHullPar)
import System.Random
import System.CPUTime
import Text.Printf
import Control.Exception (evaluate)
import Data.List (sort)
import Control.Monad (when)

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

generatePoints :: Int -> [Point2D]
generatePoints n =
  let randomVals = take (2 * max 1 n) $ randomDouble 42
      points = map (uncurry Point2D) $ zip (odds randomVals) (evens randomVals)
  in if null points 
     then [Point2D 0 0] 
     else points

timeFunction :: ([Point2D] -> [Point2D]) -> [Point2D] -> IO Double
timeFunction f points = do
  start <- getCPUTime
  _ <- evaluate (f points) 
  end <- getCPUTime
  return $ fromIntegral (end - start) / (10^(9 :: Integer)) 

compareQuickHull :: Int -> IO ()
compareQuickHull n = do
  let points = generatePoints n
  putStrLn $ show n ++ " random points"
  putStrLn $ "point generation size: " ++ show (length points)

  let seqResult = quickHull points
      parResult = quickHullPar points
  
  putStrLn $ "sequential hull size: " ++ show (length seqResult)
  putStrLn $ "parallel hull size:   " ++ show (length parResult)
  
  -- sort results for comparison and compare
  let sortedSeqResult = sort seqResult
      sortedParResult = sort parResult
  putStrLn $ "results for parallel and sequential match: " ++ show (sortedSeqResult == sortedParResult)
  
  -- if they don't match up
  when (sortedSeqResult /= sortedParResult) $ do
    putStrLn "sequential hull points:"
    mapM_ print sortedSeqResult
    putStrLn "parallel hull points:"
    mapM_ print sortedParResult
  
  -- for comparing time
  seqTime <- timeFunction quickHull points
  putStrLn $ "sequential time: " ++ printf "%.3f" seqTime ++ " ms"
  parTime <- timeFunction quickHullPar points
  putStrLn $ "parallel time:   " ++ printf "%.3f" parTime ++ " ms"
  let speedup = seqTime / parTime
  putStrLn $ "speedup:        " ++ printf "%.2f" speedup ++ "x"

main :: IO ()
main = do
  compareQuickHull 1024
  compareQuickHull 4096
  compareQuickHull 16384