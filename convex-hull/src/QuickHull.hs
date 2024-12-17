module QuickHull (quickHull2, quickHull2Par) where

import Control.DeepSeq (NFData)
import Control.Lens ((^.))
import Control.Parallel.Strategies (parList, rdeepseq, using)
import Data.Function (on)
import Data.List (maximumBy, minimumBy, partition)
import Linear.V2 (R1 (_x), R2 (_y), V2)
import Lib(distFromLine2)

-- TODO: Is this version faster for you? It's the exact same algorithm, in theory it's less efficient
-- quickHull2 :: (Ord a, Num a) => [V2 a] -> [V2 a]
-- quickHull2 points =
--   let
--     _quickHull2 :: (Num a, Ord a) => [V2 a] -> V2 a -> V2 a -> [V2 a]
--     _quickHull2 ps p0 p1
--       | (null . drop 1) onLeft = p0 : onLeft
--       | otherwise = _quickHull2 onLeft p0 pm ++ _quickHull2 onLeft pm p1
--      where
--       onLeftDists = (filter ((> 0) . snd) . map (\p -> (p, distFromLine2 p0 p1 p))) ps
--       onLeft = map fst onLeftDists
--       pm = (fst . maximumBy (compare `on` snd)) onLeftDists
--
--     pXMin = minimumBy (compare `on` (^. _x)) points
--     pXMax = maximumBy (compare `on` (^. _x)) points
--
--   in if (null . drop 3) points then points else _quickHull2 points pXMin pXMax ++ _quickHull2 points pXMax pXMin

quickHull2 :: (Ord a, Num a) => [V2 a] -> [V2 a]
quickHull2 points =
  let
    _quickHull2 :: (Num a, Ord a) => [V2 a] -> V2 a -> V2 a -> [V2 a]
    _quickHull2 ps p0 p1
      | null ps = [p1]
      | otherwise =  _quickHull2 onRight pm p1 ++ _quickHull2 onLeft p0 pm
     where
      pm = maximumBy (compare `on` distFromLine2 p0 p1) ps
      (onLeft, maybeOnRight) = partition ((> 0) . distFromLine2 p0 pm) ps
      onRight = filter ((> 0) . distFromLine2 pm p1) maybeOnRight

    pXMin = minimumBy (compare `on` (^. _x)) points
    pXMax = maximumBy (compare `on` (^. _x)) points

    (topPoints, bottomPoints) = partition ((> 0) . distFromLine2 pXMin pXMax) points

  in if (null . drop 3) points then points else _quickHull2 topPoints pXMin pXMax ++ _quickHull2 bottomPoints pXMax pXMin


quickHull2Par :: (Num a, Ord a, NFData a) => [V2 a] -> [V2 a]
quickHull2Par points =
  let maxDepth = 100
      _quickHull2Par :: (Num a, Ord a, NFData a) => Int -> [V2 a] -> (V2 a, V2 a) -> [V2 a]
      _quickHull2Par d ps (p0, p1)
        | null onLeft = [p0]
        | d < maxDepth = concat (map (_quickHull2Par (d + 1) onLeft) nextLines `using` parList rdeepseq)
        | otherwise = concatMap (_quickHull2Par (d + 1) onLeft) nextLines
       where
        onLeftDists = (filter ((> 0) . snd) . map (\p -> (p, distFromLine2 p0 p1 p))) ps
        onLeft = map fst onLeftDists
        pm = (fst . maximumBy (compare `on` snd)) onLeftDists
        nextLines = [(p0, pm), (pm, p1)]

      maxXPoint = maximumBy (compare `on` (^. _x)) points
      minXPoint = minimumBy (compare `on` (^. _x)) points
      maxYPoint = maximumBy (compare `on` (^. _y)) points
      minYPoint = minimumBy (compare `on` (^. _y)) points
      --
      topLeft = (minXPoint, maxYPoint)
      topRight = (maxYPoint, maxXPoint)
      bottomRight = (maxXPoint, minYPoint)
      bottomLeft = (minYPoint, minXPoint)

   in if (null . drop 3) points then points else concat (map (_quickHull2Par 1 points) [topLeft, topRight, bottomRight, bottomLeft] `using` parList rdeepseq)
