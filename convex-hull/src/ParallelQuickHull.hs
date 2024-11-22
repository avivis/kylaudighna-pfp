{-# LANGUAGE BangPatterns #-}

module ParallelQuickHull (quickHullPar) where
import Control.Parallel
import Control.Parallel.Strategies
import Control.DeepSeq
import Data.List (nub, maximumBy, minimumBy)
import Lib (Line2D (Line2D), Point2D (Point2D))

crossProduct :: Point2D -> Line2D -> Double
crossProduct (Point2D px py) (Line2D (Point2D x0 y0) (Point2D x1 y1)) = 
  (x0 - px) * (y1 - py) - (y0 - py) * (x1 - px)

quickHullPar_ :: [Point2D] -> Line2D -> [Point2D]
quickHullPar_ points l@(Line2D p0 p1)
  | null pointsDists = []
  | length rightOfLine < 2 = p0 : rightOfLine
  | otherwise = 
      let maxPoint = (fst . maximumBy (\(_, x) (_, y) -> compare x y)) rightOfLineDists
          (leftResult, rightResult) = 
            runEval $ do
              left <- rpar (force (quickHullPar_ rightOfLine (Line2D p0 maxPoint)))
              right <- rpar (force (quickHullPar_ rightOfLine (Line2D maxPoint p1)))
              rseq left
              rseq right
              return (left, right)
      in leftResult ++ rightResult
  where 
    pointsDists = [(p, crossProduct p l) | p <- points]
    rightOfLineDists = filter ((> 0) . snd) pointsDists
    rightOfLine = map fst rightOfLineDists
-- quickHull_ :: [Point2D] -> Line2D -> [Point2D]
-- quickHull_ points l@(Line2D p0 p1) =
--   let pointsDists = [(p, crossProduct p l) | p <- points]
--       rightOfLineDists = filter ((> 0) . snd) pointsDists
--       rightOfLine = map fst rightOfLineDists
--    in if length rightOfLineDists < 2
--         then p0 : rightOfLine
--         else
--           let maxPoint = (fst . maximumBy (\(_, x) (_, y) -> compare x y)) rightOfLineDists
--            in quickHull_ rightOfLine (Line2D p0 maxPoint) ++ quickHull_ rightOfLine (Line2D maxPoint p1)

quickHullPar :: [Point2D] -> [Point2D]
quickHullPar [] = []
quickHullPar [p] = [p]
quickHullPar points =
  let cmpX (Point2D ax _) (Point2D bx _) = compare ax bx
      maxXPoint = maximumBy cmpX points
      minXPoint = minimumBy cmpX points
      
      (leftHull, rightHull) = 
        runEval $ do
          left <- rpar (force (quickHullPar_ points (Line2D minXPoint maxXPoint)))
          right <- rpar (force (quickHullPar_ points (Line2D maxXPoint minXPoint)))
          rseq left
          rseq right
          return (left, right)
  in leftHull ++ rightHull