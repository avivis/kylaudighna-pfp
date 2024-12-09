{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module QuickHull3D ( quickHull3, quickHull3Par) where

import Control.DeepSeq (NFData)
import Control.Lens ((^.))
import Control.Parallel.Strategies (parList, rdeepseq, using)
import Data.Function (on)
import Data.List (maximumBy, minimumBy, nub, sortBy, delete)
import Linear.V3 (V3(..), cross)
import Linear.Metric (dot, norm, distance)
import Linear.V3 (R1(_x))
import Debug.Trace (trace)

signedVolume :: Floating a => V3 a -> V3 a -> V3 a -> V3 a -> a
signedVolume a b c d = 
    dot (cross (b - a) (c - a)) (d - a) / 6.0

findPointsAbove :: (Ord a, Floating a, Show a) => a -> V3 a -> V3 a -> V3 a -> [V3 a] -> [(V3 a, a)]
findPointsAbove epsilon p0 p1 p2 points = 
    let volumes = [(p, vol) | p <- points, p /= p0 && p /= p1 && p /= p2, let vol = signedVolume p0 p1 p2 p, vol > epsilon]
    in volumes

-- Sequential 
quickHull3_ :: (Ord a, Floating a, Show a) => a -> [V3 a] -> V3 a -> V3 a -> V3 a -> [(V3 a, V3 a, V3 a)]
quickHull3_ epsilon points p0 p1 p2 =
    let pointsAbove = findPointsAbove epsilon p0 p1 p2 points
    in if null pointsAbove
       then [(p0, p1, p2)]
       else let (pm, _) = maximumBy (compare `on` snd) pointsAbove
                newPoints = map fst pointsAbove
                processFace a b c pts = 
                    let abovePoints = [p | p <- pts, p /= a && p /= b && p /= c, signedVolume a b c p > epsilon]
                    in if null abovePoints
                       then [(a, b, c)]
                       else let furthest = maximumBy (compare `on` \p -> signedVolume a b c p) abovePoints
                            in quickHull3_ epsilon abovePoints a b furthest ++ quickHull3_ epsilon abovePoints b c furthest ++ quickHull3_ epsilon abovePoints c a furthest
            in processFace p0 p1 pm points ++ processFace p1 p2 pm points ++ processFace p2 p0 pm points

quickHull3 :: (Ord a, Floating a, Show a) => [V3 a] -> [V3 a]
quickHull3 points 
    | length points < 4 = points
    | otherwise = 
        let epsilon = 1e-8
            p0 = minimumBy (compare `on` (^._x)) points
            p1 = maximumBy (compare `on` distance p0) points
            rest1 = filter (\p -> p /= p0 && p /= p1) points
            p2 = maximumBy (compare `on` \p -> norm (cross (p1 - p0) (p - p0))) rest1
            rest2 = filter (\p -> p /= p2) rest1
            p3 = maximumBy (compare `on` \p -> abs $ signedVolume p0 p1 p2 p) rest2
            initialFaces = [(p0, p1, p2), (p0, p2, p3), (p0, p3, p1), (p1, p3, p2)]
            remainingPoints = filter (\p -> p /= p0 && p /= p1 && p /= p2 && p /= p3) points
            processFace (a, b, c) = 
                let abovePoints = [p | p <- remainingPoints, signedVolume a b c p > epsilon]
                in if null abovePoints
                   then [(a, b, c)]
                   else quickHull3_ epsilon abovePoints a b c
            allTriangles = concatMap processFace initialFaces
            result = nub $ concatMap (\(a,b,c) -> [a,b,c]) allTriangles
        in result

-- Parallel 
quickHull3Par :: forall a. (Ord a, Floating a, NFData a, Show a) => [V3 a] -> [V3 a]
quickHull3Par points = 
    if length points < 4 
    then points 
    else result
  where
    maxDepth :: Int
    maxDepth = 100
    
    epsilon :: a
    epsilon = 1e-8
    
    processPoints :: (Ord a, Floating a, NFData a, Show a) => 
                    Int -> [V3 a] -> V3 a -> V3 a -> V3 a -> [(V3 a, V3 a, V3 a)]
    processPoints d pts p0' p1' p2' = 
        let pointsAbove = findPointsAbove epsilon p0' p1' p2' pts
        in if null pointsAbove || d >= maxDepth
           then [(p0', p1', p2')]
           else let (pm, _) = maximumBy (compare `on` snd) pointsAbove
                    abovePoints = [p | p <- pts, p /= p0' && p /= p1' && p /= p2', signedVolume p0' p1' p2' p > epsilon]
                    nextFaces = [(p0', p1', pm), (p1', p2', pm), (p2', p0', pm)]
                    processFace (a, b, c) = processPoints (d + 1) abovePoints a b c
                in if d < maxDepth `div` 2
                   then (concat (map processFace nextFaces)) `using` parList rdeepseq
                   else concatMap processFace nextFaces

    p0 = minimumBy (compare `on` (^._x)) points
    p1 = maximumBy (compare `on` distance p0) points
    rest1 = filter (\p -> p /= p0 && p /= p1) points
    p2 = maximumBy (compare `on` \p -> norm (cross (p1 - p0) (p - p0))) rest1
    rest2 = filter (\p -> p /= p2) rest1
    p3 = maximumBy (compare `on` \p -> abs $ signedVolume p0 p1 p2 p) rest2
    initialFaces = [(p0, p1, p2), (p0, p2, p3), (p0, p3, p1), (p1, p3, p2)]
    remainingPoints = filter (\p -> p /= p0 && p /= p1 && p /= p2 && p /= p3) points
    allTriangles = (concat (map (\(a,b,c) -> processPoints 1 remainingPoints a b c) initialFaces)) 
                   `using` parList rdeepseq
    result = nub $ concatMap (\(a,b,c) -> [a,b,c]) allTriangles