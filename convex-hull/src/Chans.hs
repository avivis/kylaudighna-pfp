module Chans (jarvisMarch, chans2, chans2Par) where

import Control.Lens ((^.))
import Control.Parallel.Strategies (NFData, parMap, rdeepseq, parBuffer, withStrategy)
import Data.Function (on)
import Data.List (maximumBy, minimumBy)
import Data.List.Split (chunksOf)
import qualified Data.Vector as V
import GHC.Float (sqrtDouble)
import Lib (orientation, sortPointsCCW)
import Linear.V2 (R1 (_x), V2)
import QuickHull (quickHull2)

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

rightmostCCWPoint :: (Ord a, Num a) => V2 a -> V.Vector (V2 a) -> V2 a
rightmostCCWPoint o ps = ps V.! binarySearch 0 (V.length ps - 1) lPrevInit lNextInit
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
chans2 ps = _chans2 start
 where
  m = (floor . sqrtDouble . fromIntegral) (length ps) -- TODO: I think this is a good approximation, does anyone have a better one?
  subPoints = chunksOf m ps
  subHulls = map (V.fromList . sortPointsCCW . quickHull2) subPoints
  start = minimumBy (compare `on` (^. _x)) [V.minimumOn (^. _x) subHull | subHull <- subHulls] -- Point across all hulls with lowest X
  _chans2 p =
    let next = maximumBy (orientation p) $ map (rightmostCCWPoint p . V.filter (/= p)) subHulls -- Eval all rightmost points in parallel
     in if next == start then [p] else p : _chans2 next

chans2Par :: (Ord a, Num a, NFData a) => [V2 a] -> [V2 a]
chans2Par ps = _chans2Par start
 where
  m = (floor . sqrtDouble . fromIntegral) (length ps) -- TODO: Remove length, I just realized that it adds a ton of time
  subPoints = chunksOf m ps
  subHulls = withStrategy (parBuffer 32 rdeepseq) (map (V.fromList . sortPointsCCW . quickHull2) subPoints) -- TODO: Play around with making this quickHull2Par, I found it was about the same speed
  start = minimumBy (compare `on` (^. _x)) [V.minimumOn (^. _x) subHull | subHull <- subHulls] -- Point across all hulls with lowest X
  _chans2Par p =
    let next = maximumBy (orientation p) $ map (rightmostCCWPoint p . V.filter (/= p)) subHulls -- Eval all rightmost points in parallel
     in if next == start then [p] else p : _chans2Par next
