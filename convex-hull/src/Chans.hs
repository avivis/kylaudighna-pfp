module Chans (jarvisMarch, chans2, chans2Par) where

import Control.Lens ((^.))
import Control.Parallel.Strategies (NFData, parBuffer, rdeepseq, withStrategy)
import Data.List (maximumBy, minimumBy)
import Data.List.Split (chunksOf)
import qualified Data.Vector as V
import GHC.Float (sqrtDouble)
import Lib (distFromLine2, orientation)
import Linear.V2 (R1 (_x), V2)
import Data.Ord(comparing)
-- import qualified Data.Vector.Mutable as MV
-- import Control.Monad(when)
-- import Control.Monad.ST (ST, runST)
-- import Data.STRef (newSTRef, readSTRef, writeSTRef)

jarvisMarch :: (Ord a, Num a) => [V2 a] -> [V2 a]
jarvisMarch [] = []
jarvisMarch p@[_] = p
jarvisMarch p@[_, _] = p
jarvisMarch p@[_, _, _] = p
jarvisMarch points = _jarvisMarch start
 where
  start = minimum points
  _jarvisMarch p =
    let next = (minimumBy (orientation p) . filter (/= p)) points
     in if next == start then [p] else p : _jarvisMarch next

-- inPlacePartition :: MV.MVector s (V2 a) -> V.Vector (V2 a) -> (V2 a -> Bool) -> ST s ()
-- inPlacePartition buffer ps predicate = do
--   i <- newSTRef 0 -- Index
--   V.forM_ ps $ \p -> do
--     -- When the predicate is True...
--     when (predicate p) $ do
--       iPrev <- readSTRef i
--       MV.write buffer iPrev p
--       writeSTRef i (iPrev + 1)
--   size <- readSTRef i
--   return V.freeze (MV.slice 0 size buffer)
--
-- quickHull2M :: (Ord a, Num a) => V.Vector (V2 a) -> V.Vector (V2 a)
-- quickHull2M points
--   | V.length points < 4 = points
--   | otherwise =
--       runST $ do
--         let pXMax = V.maximumBy (comparing (^. _x)) points
--             pXMin = V.minimumBy (comparing (^. _x)) points
--         let (topPoints, bottomPoints) = V.partition ((> 0) . distFromLine2 pXMin pXMax) points
--
--         -- Create reusable mutable buffers
--         buffer <- MV.new (V.length points)
--
--         -- Perform recursive QuickHull with buffer reuse
--         topHull <- _quickHull2 buffer topPoints pXMin pXMax
--         bottomHull <- _quickHull2 buffer bottomPoints pXMax pXMin
--         return $ topHull V.++ bottomHull
--   where
--     _quickHull2 :: (Num a, Ord a) => MV.MVector s (V2 a) -> V.Vector (V2 a) -> V2 a -> V2 a -> ST s (V.Vector (V2 a))
--     _quickHull2 buffer ps p0 p1
--       | V.null ps = return $ V.singleton p1
--       | otherwise = do
--           let pm = V.maximumBy (comparing (distFromLine2 p0 p1)) ps
--
--           -- Partition points with buffer reuse
--           (onLeft, leftSize) <- partitionWithBuffer buffer ps (\pt -> distFromLine2 p0 pm pt > 0)
--           -- Recursive calls with the same buffer
--           leftHull <- _quickHull2 buffer (V.freeze $ MV.slice 0 leftSize onLeft) p0 pm
--           (onRight, _) <- partitionWithBuffer buffer ps (\pt -> distFromLine2 pm p1 pt > 0)
--           rightHull <- _quickHull2 buffer (MV.slice 0 leftSize onRight) pm p1
--           return $ leftHull V.++ rightHull
--
-- -- Partition points using the same buffer
-- partitionWithBuffer :: (Num a, Ord a) => MV.MVector s (V2 a) -> V.Vector (V2 a) -> (V2 a -> Bool) -> ST s (MV.MVector s (V2 a), Int)
-- partitionWithBuffer buffer points predicate = do
--   sizeRef <- newSTRef 0
--   V.forM_ points $ \pt -> do
--     when (predicate pt) $ do
--       size <- readSTRef sizeRef
--       MV.write buffer size pt
--       writeSTRef sizeRef (size + 1)
--   size <- readSTRef sizeRef
--   return (buffer, size)

-- Special vectorized version of quickHull, since with sub-chunks it's fairly cheap to create the smaller sub-point vectors
quickHull2 :: (Ord a, Num a) => V.Vector (V2 a) -> V.Vector (V2 a)
quickHull2 points =
  let
    _quickHull2 :: (Num a, Ord a) => V.Vector (V2 a) -> V2 a -> V2 a -> V.Vector (V2 a)
    _quickHull2 ps p0 p1
      | V.null ps = V.singleton p1
      | otherwise = _quickHull2 onRight pm p1 V.++ _quickHull2 onLeft p0 pm
     where
      pm = V.maximumOn (distFromLine2 p0 p1) ps
      (onLeft, onRightOrCenter) = V.partition ((> 0) . distFromLine2 p0 pm) ps
      onRight = V.filter ((> 0) . distFromLine2 pm p1) onRightOrCenter

    pXMax = V.maximumOn (^. _x) points
    pXMin = V.minimumOn (^. _x) points

    (topPoints, bottomPoints) = V.partition ((> 0) . distFromLine2 pXMin pXMax) points
   in
    if V.length points < 4 then points else _quickHull2 bottomPoints pXMax pXMin V.++ _quickHull2 topPoints pXMin pXMax

leftmostPoint :: (Ord a, Num a) => V2 a -> V.Vector (V2 a) -> V2 a
leftmostPoint o ps = ps V.! binarySearch 0 (V.length ps - 1) lPrevInit lNextInit
 where
  compareAdjacent i = (prev, next)
   where
    prev = orientation o (ps V.! i) (ps V.! ((i - 1) `mod` V.length ps))
    next = orientation o (ps V.! i) (ps V.! ((i + 1) `mod` V.length ps))

  -- We can save some computations here if we pass the orientations of the leftmost node, since those only need to be recalculated when l changes
  (lPrevInit, lNextInit) = compareAdjacent 0

  binarySearch l r lPrevOri lNextOri
    -- If we can't reach anymore points, we'll do l
    | l >= r = l
    -- If o->m->m-1 is not a CCW turn, and o->m->m+1 is a CCW turn, we are at the rightmost point!
    | mPrevOri /= LT && mNextOri == GT = m
    -- If o->l->m is a CCW turn and (o->l->l+1 is a CW turn or o->l->l-1 and o->l->l+1 turn the same way), or o->l->m is a CW turn and o->m->m-1 is a CW turn, then search to the right of m (from l to m)
    | mLOri == GT && (lNextOri == LT || lPrevOri == lNextOri) || mLOri == LT && mPrevOri == LT = binarySearch l m lPrevOri lNextOri
    -- Otherwise, search to the left of m (from m+1 to r) (new lPrev = -mNext, new lNext needs to be set manually)
    | otherwise = binarySearch (m + 1) r (compare EQ mNextOri) (orientation o (ps V.! (m + 1)) (ps V.! ((m + 2) `mod` V.length ps)))
   where
    m = (l + r) `div` 2
    mLOri = orientation o (ps V.! l) (ps V.! m) -- Which side is m on, relative to l?
    (mPrevOri, mNextOri) = compareAdjacent m -- What are the orientations of o->m->m-1 and o->m->m+1?

chans2 :: (Ord a, Num a) => Int -> [V2 a] -> [V2 a]
chans2 n ps = _chans2 start
 where
  m = 3 * (floor . sqrtDouble . fromIntegral) n -- 3 * sqrt(A) is a good approximation of the perimeter of a polygon with area A
  subPoints = chunksOf m ps
  subHulls = map (quickHull2 . V.fromList) subPoints
  start = minimumBy (comparing (^. _x)) $ map (V.minimumOn (^. _x)) subHulls -- Point across all hulls with lowest X
  _chans2 p =
    let next = maximumBy (orientation p) $ map (leftmostPoint p . V.filter (/= p)) subHulls
     in if next == start then [p] else p : _chans2 next

chans2Par :: (Ord a, Num a, NFData a) => Int -> [V2 a] -> [V2 a]
chans2Par n ps = _chans2Par start
 where
  m = 3 * (floor . sqrtDouble . fromIntegral) n
  subPoints = chunksOf m ps
  subHulls = withStrategy (parBuffer 64 rdeepseq) (map (quickHull2 . V.fromList) subPoints) -- TODO: Play around with making this quickHull2Par, I found it was about the same speed
  start = minimumBy (comparing (^. _x)) $ map (V.minimumOn (^. _x)) subHulls -- Point across all hulls with lowest X
  _chans2Par p =
    let next = maximumBy (orientation p) $ map (leftmostPoint p . V.filter (/= p)) subHulls
     in if next == start then [p] else p : _chans2Par next
