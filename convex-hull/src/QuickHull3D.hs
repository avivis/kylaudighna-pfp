module QuickHull3D ( quickHull3, quickHull3Par) where

import Control.DeepSeq (NFData)
import Control.Lens ((^.))
import Control.Parallel.Strategies (parList, rdeepseq, using)
import Data.Function (on)
import Data.List (maximumBy, minimumBy, nub)
import Linear.V3 (V3(..), cross)
import Linear.Metric (dot, norm)
import Linear.V3 (R1(_x))
import Debug.Trace (trace)

-- find points above plane
findPointsAbove :: (Ord a, Floating a, Show a) => a -> V3 a -> V3 a -> V3 a -> [V3 a] -> [(V3 a, a)]
findPointsAbove epsilon p0 p1 p2 points = filter ((> epsilon) . snd) $ [(p, height) | p <- points, let height = dot (cross (p1 - p0) (p2 - p0)) (p - p0) / norm (cross (p1 - p0) (p2 - p0))]

-- sequential
quickHull3_ :: (Ord a, Floating a, Show a) => [V3 a] -> V3 a -> V3 a -> V3 a -> [(V3 a, V3 a, V3 a)]
quickHull3_ points p0 p1 p2 = trace ("processing triangle: " ++ show (p0, p1, p2)) $
    let pointsHeights = findPointsAbove epsilon p0 p1 p2 points
        epsilon = 1e-10
    in if null pointsHeights
       then trace "no points above" [(p0, p1, p2)]
       else let pm = fst $ maximumBy (compare `on` snd) pointsHeights
                newPoints = map fst pointsHeights
            in trace ("found furthest point: " ++ show pm) $ quickHull3_ newPoints p0 p1 pm ++ quickHull3_ newPoints p1 p2 pm ++ quickHull3_ newPoints p2 p0 pm
quickHull3 :: (Ord a, Floating a, Show a) => [V3 a] -> [V3 a]
quickHull3 points = trace ("sequential QuickHull3D with " ++ show (length points) ++ " points") $
    if length points < 4 
    then trace "less than 4 points, returning as is" points
    else let
        -- most distant point pair for initial edge
        p0 = minimumBy (compare `on` (^._x)) points
        p1 = maximumBy (compare `on` distanceFrom p0) points
        -- point farthest from p0-p1 line
        p2 = maximumBy (compare `on` \p -> abs $ dot (cross (p1 - p0) (p - p0)) (cross (p1 - p0) (p - p0))) $ filter (\p -> p /= p0 && p /= p1) points
        -- three points to build initial tetrahedron
        rest = filter (\p -> p /= p0 && p /= p1 && p /= p2) points
        p3 = maximumBy (compare `on` \p -> abs $ dot (cross (p1 - p0) (p2 - p0)) (p - p0)) rest
        -- initial faces
        initialFaces = [(p0, p1, p2), (p0, p2, p3), (p0, p3, p1), (p1, p3, p2)]
        allTriangles = concatMap (\(a,b,c) -> quickHull3_ points a b c) initialFaces
        result = nub $ concatMap (\(a,b,c) -> [a,b,c]) allTriangles in trace ("Found " ++ show (length result) ++ " vertices") result
  where
    distanceFrom :: Num a => V3 a -> V3 a -> a
    distanceFrom ref p = dot (p - ref) (p - ref)

-- parallel
quickHull3Par :: (Ord a, Floating a, NFData a, Show a) => [V3 a] -> [V3 a]
quickHull3Par points | length points < 4 = points | otherwise = result
  where
    maxDepth :: Int
    maxDepth = 100
    epsilon :: Floating a => a
    epsilon = 1e-10
    quickHull3Par_ :: (Ord a, Floating a, NFData a, Show a) => Int -> [V3 a] -> (V3 a, V3 a, V3 a) -> [(V3 a, V3 a, V3 a)]
    quickHull3Par_ d pts face@(p0', p1', p2') = 
        let pointsHeights = findPointsAbove epsilon p0' p1' p2' pts in if null pointsHeights || d >= maxDepth
           then [face]
           else let pm = fst $ maximumBy (compare `on` snd) pointsHeights
                    newPoints = map fst pointsHeights
                    nextFaces = [(p0', p1', pm), (p1', p2', pm), (p2', p0', pm)]
                    processFace f = quickHull3Par_ (d + 1) newPoints f
                in if d < maxDepth `div` 2
                   then concat (map processFace nextFaces `using` parList rdeepseq)
                   else concatMap processFace nextFaces
    p0 = minimumBy (compare `on` (^._x)) points
    p1 = maximumBy (compare `on` distanceFrom p0) points
    p2 = maximumBy (compare `on` \p -> abs $ dot (cross (p1 - p0) (p - p0)) (cross (p1 - p0) (p - p0))) $ filter (\p -> p /= p0 && p /= p1) points
    rest = filter (\p -> p /= p0 && p /= p1 && p /= p2) points
    p3 = maximumBy (compare `on` \p -> abs $ dot (cross (p1 - p0) (p2 - p0)) (p - p0)) rest
    initialFaces = [(p0, p1, p2), (p0, p2, p3), (p0, p3, p1), (p1, p3, p2)]
    allTriangles = concat (map (quickHull3Par_ 1 points) initialFaces `using` parList rdeepseq)
    result = nub $ concatMap (\(a,b,c) -> [a,b,c]) allTriangles
    distanceFrom :: Num a => V3 a -> V3 a -> a
    distanceFrom ref p = dot (p - ref) (p - ref)