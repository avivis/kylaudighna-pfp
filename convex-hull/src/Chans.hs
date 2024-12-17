module Chans (jarvisMarch, chans2, chans2Par) where

import Control.Lens ((^.))
import Control.Parallel.Strategies (NFData, parBuffer, rdeepseq, using)
import Data.Function (on)
import Data.List (maximumBy, minimumBy)
import Data.List.Split (chunksOf)
import qualified Data.Vector as V
import GHC.Float (sqrtDouble)
import Lib (orientation)
import QuickHull (quickHull2)
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

chansJarvisMarch :: (Num a, Ord a) => [V.Vector (V2 a)] -> V2 a -> V2 a -> [V2 a]
chansJarvisMarch subHulls start p =
  let next = maximumBy (orientation p) $ map (rightmostPoint p) subHulls
    in if next == start then [p] else p : chansJarvisMarch subHulls start next

rightmostPoint :: (Ord a, Num a) => V2 a -> V.Vector (V2 a) -> V2 a
rightmostPoint o ps = ps V.! binarySearch 0 (V.length ps - 1) lPrevInit lNextInit
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
    -- If o->m->m-1 is not a CCW turn, and o->m->m+1 is not CCW turn, we are at the rightmost point!
    | mPrevOri /= LT && mNextOri /= LT = m
    -- If o->l->m is a CCW turn and (o->l->l+1 is a CW turn or o->l->l-1 and o->l->l+1 turn the same way), or o->l->m is a CW turn and o->m->m-1 is a CW turn, then search to the right of m (from l to m)
    | mLOri == GT && (lNextOri == LT || lPrevOri == lNextOri) || mLOri == LT && mPrevOri == LT = binarySearch l m lPrevOri lNextOri
    -- Otherwise, search to the left of m (from m+1 to r) (new lPrev = -mNext, new lNext needs to be set manually)
    | otherwise = binarySearch (m + 1) r (compare EQ mNextOri) (orientation o (ps V.! (m + 1)) (ps V.! ((m + 2) `mod` V.length ps)))
   where
    m = (l + r) `div` 2
    mLOri = orientation o (ps V.! l) (ps V.! m) -- Which side is m on, relative to l?
    (mPrevOri, mNextOri) = compareAdjacent m -- What are the orientations of o->m->m-1 and o->m->m+1?

chans2 :: (Ord a, Num a) => Int -> [V2 a] -> [V2 a]
chans2 n ps = chansJarvisMarch subHulls start start
 where
  m = 3 * (floor . sqrtDouble . fromIntegral) n -- 3 * sqrt(A) is a good approximation of the perimeter of a polygon with area A
  subPoints = chunksOf m ps
  subHulls = map (V.force . V.fromList . quickHull2) subPoints -- force means we save on space
  start = minimumBy (compare `on` (^. _x)) $ map (V.minimumOn (^. _x)) subHulls

chans2Par :: (Ord a, Num a, NFData a) => Int -> [V2 a] -> [V2 a]
chans2Par n ps = chansJarvisMarch subHulls start start
 where
  m = 3 * (floor . sqrtDouble . fromIntegral) n
  subPoints = chunksOf m ps
  subHulls = map (V.force . V.fromList . quickHull2) subPoints `using` parBuffer 32 rdeepseq -- Here's the parallelization
  start = minimumBy (compare `on` (^. _x)) $ map (V.minimumOn (^. _x)) subHulls
