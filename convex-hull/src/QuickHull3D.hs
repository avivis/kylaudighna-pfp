module QuickHull3D (quickHull3, quickHull3Par) where
import Control.DeepSeq (NFData)
import Control.Lens ((^.))
import Control.Parallel.Strategies (parList, rdeepseq, using)
import Data.Function (on)
import Data.List (maximumBy, minimumBy, nub, partition)
import Linear.V3 (V3(..), cross)
import Linear.Metric (dot, norm, distance)
import Linear.V3 (R1(_x))

-- Face representation as described in Section 1: "We represent a convex hull with a set of facets and a set of adjacency lists"
data Face a = Face {vertices :: (V3 a, V3 a, V3 a), outsideSet :: [(V3 a, a)], furthestPoint :: Maybe (V3 a, a)}

-- Based on geometric orientation test described in Section 2: Signed volume calculation for determining if point is above face
signedVolume :: Floating a => V3 a -> V3 a -> V3 a -> V3 a -> a
signedVolume a b c d = dot (cross (b - a) (c - a)) (d - a) / 6.0

-- find points above a plane with distance, implementing the "outside set" concept from Section 2: "A point is in a facet's outside set only if it is above the facet"
findPointsAbove :: (Ord a, Floating a) => a -> V3 a -> V3 a -> V3 a -> [V3 a] -> [(V3 a, a)]
findPointsAbove epsilon p0 p1 p2 points = let volumes = [(p, vol) | p <- points, p /= p0 && p /= p1 && p /= p2, let vol = signedVolume p0 p1 p2 p, vol > epsilon] in volumes

-- Create initial faces of tetrahedron with their outside sets from Section 2: "Quickhull starts with the convex hull of d + 1 points"
createInitialFaces :: (Ord a, Floating a) => a -> [V3 a] -> V3 a -> V3 a -> V3 a -> V3 a -> [Face a]
createInitialFaces epsilon points p0 p1 p2 p3 = 
    let faces = [(p0, p1, p2), (p0, p2, p3), (p0, p3, p1), (p1, p3, p2)]
        remainingPoints = filter (\p -> p /= p0 && p /= p1 && p/= p2 && p /= p3) points
        createFace (v1, v2, v3) = 
            let outside = findPointsAbove epsilon v1 v2 v3 remainingPoints
                furthest = if null outside then Nothing else Just $ maximumBy (compare `on` snd) outside
            in Face (v1, v2, v3) outside furthest
    in map createFace faces

-- Process a face using Beneath-Beyond method described in Section 1: "The Beneath-Beyond Algorithm repeatedly adds a point to the convex hull of the previously processed points"
processFace :: (Ord a, Floating a) => a -> Face a -> [V3 a] -> [(V3 a, V3 a, V3 a)]
processFace epsilon face allPoints =
    case furthestPoint face of
        Nothing -> [vertices face]
        Just (p, _) -> 
            let (v1, v2, v3) = vertices face
                -- Create cone of new faces (Section 1)
                newFaces = [(v1, v2, p), (v2, v3, p), (v3, v1, p)]
                remainingPoints = map fst $ filter ((/= p) . fst) $ outsideSet face
                processNewFace (a, b, c) = 
                    let outside = findPointsAbove epsilon a b c remainingPoints
                    in if null outside
                       then [(a, b, c)]
                       else let furthest = maximumBy (compare `on` snd) outside
                            in processNewFaceWithPoints epsilon outside furthest (a, b, c)
            in concatMap processNewFace newFaces


-- Process new faces recursively with their outside sets
processNewFaceWithPoints :: (Ord a, Floating a) => a -> [(V3 a, a)] -> (V3 a, a) -> (V3 a, V3 a, V3 a) -> [(V3 a, V3 a, V3 a)]
processNewFaceWithPoints epsilon points furthest (a, b, c) =
    let (fp, _) = furthest
        remaining = map fst $ filter ((/= fp) . fst) points
    in if null remaining
       then [(a, b, c)]
       else let newFaces = [(a, b, fp), (b, c, fp), (c, a, fp)]
                processSubFace (v1, v2, v3) =
                    let above = findPointsAbove epsilon v1 v2 v3 remaining
                    in if null above
                       then [(v1, v2, v3)]
                       else let (p', _) = maximumBy (compare `on` snd) above
                            in processNewFaceWithPoints epsilon above (p', snd furthest) (v1, v2, v3)
            in concatMap processSubFace newFaces

-- Sequential
quickHull3 :: (Ord a, Floating a, Show a) => [V3 a] -> [V3 a]
quickHull3 points 
    | length points < 4 = points
    | otherwise = 
        let epsilon = 1e-8 -- Numerical tolerance from Section 4
            -- Initial point selection as described in Section 2
            p0 = minimumBy (compare `on` (^._x)) points
            p1 = maximumBy (compare `on` distance p0) points
            rest1 = filter (\p -> p /= p0 && p /= p1) points
            p2 = maximumBy (compare `on` \p -> norm (cross (p1 - p0) (p - p0))) rest1
            rest2 = filter (\p -> p /= p2) rest1
            p3 = maximumBy (compare `on` \p -> abs $ signedVolume p0 p1 p2 p) rest2
            initialFaces = createInitialFaces epsilon points p0 p1 p2 p3
            allTriangles = concatMap (\f -> processFace epsilon f points) initialFaces
        in nub $ concatMap (\(a,b,c) -> [a,b,c]) allTriangles

-- Parallel face processing based on Section 3's discussion of algorithm variations
processPointsParallel :: (Ord a, Floating a, NFData a, Show a) => Int -> a -> [V3 a] -> Face a -> [(V3 a, V3 a, V3 a)]
processPointsParallel depth epsilon points face =
    case furthestPoint face of
        Nothing -> [vertices face]
        Just (p, _) -> 
            let (v1, v2, v3) = vertices face
                newFaces = [(v1, v2, p), (v2, v3, p), (v3, v1, p)]
                remainingPoints = map fst $ filter ((/= p) . fst) $ outsideSet face
                processNewFace (a, b, c) = 
                    let outside = findPointsAbove epsilon a b c remainingPoints
                        newFace = Face (a,b,c) outside (if null outside 
                                                      then Nothing 
                                                      else Just $ maximumBy (compare `on` snd) outside)
                    in if depth < 50
                       then processPointsParallel (depth + 1) epsilon points newFace
                       else processFace epsilon newFace points
            in if depth < 2
               then concat (map processNewFace newFaces `using` parList rdeepseq)
               else concatMap processNewFace newFaces

-- Parallel
quickHull3Par :: (Ord a, Floating a, NFData a, Show a) => [V3 a] -> [V3 a]
quickHull3Par points 
    | length points < 4 = points
    | otherwise = 
        let epsilon = 1e-8
            p0 = minimumBy (compare `on` (^._x)) points
            p1 = maximumBy (compare `on` distance p0) points
            rest1 = filter (\p -> p /= p0 && p /= p1) points
            p2 = maximumBy (compare `on` \p -> norm (cross (p1 - p0) (p - p0))) rest1
            rest2 = filter (\p -> p /= p2) rest1
            p3 = maximumBy (compare `on` \p -> abs $ signedVolume p0 p1 p2 p) rest2
            initialFaces = createInitialFaces epsilon points p0 p1 p2 p3
            allTriangles = concat (map (processPointsParallel 0 epsilon points) initialFaces `using` parList rdeepseq)
        in nub $ concatMap (\(a,b,c) -> [a,b,c]) allTriangles