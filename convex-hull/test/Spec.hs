module Main (main) where

--
-- import Lib
--
-- main :: IO ()
-- main = someFunc

import Chans (chans2, chans2Par)
import GrahamScan (grahamScan)
import Linear.V2 (V2 (V2))

-- import Linear.V3 (V3 (V3))

import GHC.Base (assert)
import Lib (isCCWTurn, sortPointsCCW)
import QuickHull (quickHull2, quickHull2Par)
import System.Random

verifyConvexHull2Algorithm :: (RealFloat a) => [V2 a] -> ([V2 a] -> [V2 a]) -> Bool
verifyConvexHull2Algorithm ps f =
  let verifyCCWHull2 [] = True
      verifyCCWHull2 [_] = True
      verifyCCWHull2 (a : rest@(b : _)) = all (isCCWTurn a b) ps && verifyCCWHull2 rest
   in (verifyCCWHull2 . sortPointsCCW . f) ps

randomV2s :: (RandomGen g, Random a) => g -> [V2 a]
randomV2s gen =
  let (x, gen') = random gen
      (y, gen'') = random gen'
   in V2 x y : randomV2s gen''

-- randomV3s :: (RandomGen g, Random a) => g -> [V3 a]
-- randomV3s gen =
--   let (x, gen') = random gen
--       (y, gen'') = random gen'
--       (z, gen''') = random gen''
--    in V3 x y z : randomV3s gen'''

main :: IO ()
main = do
  let points = take 65536 $ randomV2s (mkStdGen 3) :: [V2 Double]
  assert (verifyConvexHull2Algorithm points grahamScan) return ()
  --
  assert (verifyConvexHull2Algorithm points quickHull2) return ()
  assert (verifyConvexHull2Algorithm points quickHull2Par) return ()
  --
  assert (verifyConvexHull2Algorithm points chans2) return ()
  assert (verifyConvexHull2Algorithm points chans2Par) return ()
