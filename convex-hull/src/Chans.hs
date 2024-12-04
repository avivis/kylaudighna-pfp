module Chans (jarvisMarch, chans2, chans2Par) where

import Control.Lens ((^.))
import Control.Parallel.Strategies (NFData, parBuffer, rdeepseq, withStrategy)
import Data.Function (on)
import Data.List (maximumBy, minimumBy)
import Data.List.Split (chunksOf)
import qualified Data.Vector as V
import GHC.Float (sqrtDouble)
import Lib (distFromLine2, orientation)
import Linear.V2 (R1 (_x), V2)

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

-- Special vectorized version of quickHull, since with sub-chunks it's fairly cheap to create the smaller sub-point vectors
quickHull2 :: (Ord a, Num a) => V.Vector (V2 a) -> V.Vector (V2 a)
quickHull2 points =
  let
    _quickHull2 :: (Num a, Ord a) => V.Vector (V2 a) -> V2 a -> V2 a -> V.Vector (V2 a)
    _quickHull2 v p0 p1
      | null v = V.singleton p1
      | otherwise = _quickHull2 onRight pm p1 V.++ _quickHull2 onLeft p0 pm
     where
      pm = maximumBy (compare `on` distFromLine2 p0 p1) v
      (onLeft, onRightOrCenter) = V.partition ((> 0) . distFromLine2 p0 pm) v
      onRight = V.filter ((> 0) . distFromLine2 pm p1) onRightOrCenter

    pXMax = maximum points
    pXMin = minimum points

    (topPoints, bottomPoints) = V.partition ((> 0) . distFromLine2 pXMin pXMax) points
   in
    if length points < 4 then points else _quickHull2 bottomPoints pXMax pXMin V.++ _quickHull2 topPoints pXMin pXMax

leftmostPoint :: (Ord a, Num a) => V2 a -> V.Vector (V2 a) -> V2 a
leftmostPoint o ps = ps V.! binarySearch 0 (V.length ps - 1) lPrevInit lNextInit
 where
  compareAdjacent i = (prev, next)
   where
    prev = orientation o (ps V.! i) (ps V.! ((i - 1) `mod` V.length ps))
    next = orientation o (ps V.! i) (ps V.! ((i + 1) `mod` V.length ps))

  -- We can save some computations here if we pass the orientations of the leftmost node, since those only need to be recalculated when l changes
  (lPrevInit, lNextInit) = compareAdjacent 0

  binarySearch l r lPrev lNext
    -- If we can't reach anymore points, we'll do l
    | l >= r = l
    -- If o->m->m-1 is not a CCW turn, and o->m->m+1 is a CCW turn, we are at the rightmost point!
    | mPrev /= LT && mNext == GT = m
    -- If o->l->m is a CCW turn and (o->l->l+1 is a CW turn or o->l->l-1 and o->l->l+1 turn the same way), or o->l->m is a CW turn and o->m->m-1 is a CW turn, then search to the right of m (from l to m)
    | mSide == GT && (lNext == LT || lPrev == lNext) || mSide == LT && mPrev == LT = binarySearch l m lPrev lNext
    -- Otherwise, search to the left of m (from m+1 to r) (new lPrev = -mNext, new lNext needs to be set manually)
    | otherwise = binarySearch (m + 1) r (compare EQ mNext) (orientation o (ps V.! (m + 1)) (ps V.! ((m + 2) `mod` V.length ps)))
   where
    m = (l + r) `div` 2
    mSide = orientation o (ps V.! l) (ps V.! m) -- Which side is m on, relative to l?
    (mPrev, mNext) = compareAdjacent m -- What are the orientations of o->m->m-1 and o->m->m+1?

chans2 :: (Ord a, Num a) => [V2 a] -> [V2 a]
chans2 l = _chans2 start
 where
  m = (floor . sqrtDouble . fromIntegral) (length l) -- TODO: I think this is a good approximation, does anyone have a better one?
  subPoints = chunksOf m l
  subHulls = map (quickHull2 . V.fromList) subPoints
  start = minimumBy (compare `on` (^. _x)) $ map (minimumBy (compare `on` (^. _x))) subHulls -- Point across all hulls with lowest X
  _chans2 p =
    let next = maximumBy (orientation p) $ map (leftmostPoint p . V.filter (/= p)) subHulls
     in if next == start then [p] else p : _chans2 next

chans2Par :: (Ord a, Num a, NFData a) => V.Vector (V2 a) -> V.Vector (V2 a)
chans2Par ps = _chans2Par start
 where
  n = V.length ps
  m = floor . sqrtDouble . fromIntegral $ n
  subPoints = chunkVector m ps
  subHulls = V.fromList $ withStrategy (parBuffer 32 rdeepseq) (V.toList $ V.map quickHull2 subPoints)
  start = V.minimumBy (compare `on` (^. _x)) $ V.map (V.minimumBy (compare `on` (^. _x))) subHulls -- Point across all hulls with lowest X
  _chans2Par p =
    let next = V.maximumBy (orientation p) $ V.map (leftmostPoint p . V.filter (/= p)) subHulls
     in if next == start then V.singleton p else V.cons p (_chans2Par next)
  chunkVector chunkSize vec
    | V.null vec = V.empty
    | otherwise =
        let (chunk, rest) = V.splitAt chunkSize vec
        in V.cons chunk (chunkVector chunkSize rest)


