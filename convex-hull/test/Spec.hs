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

verifyHull2 :: (RealFloat a) => [V2 a] -> Bool
verifyHull2 [] = True
verifyHull2 [_] = True
verifyHull2 [_, _] = True
verifyHull2 [_, _, _] = True
verifyHull2 hull =
  let sortedHull = sortPointsCCW hull
      _verifyHull2 [] = True
      _verifyHull2 [_] = True
      _verifyHull2 (a : rest@(b : _)) = all (isCCWTurn a b) hull && _verifyHull2 rest
   in _verifyHull2 sortedHull

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
  let points = take 2097152 $ randomV2s (mkStdGen 3) :: [V2 Double]
  assert (verifyHull2 (grahamScan points)) return ()

  assert (verifyHull2 (quickHull2 points)) return ()
  assert (verifyHull2 (quickHull2Par points)) return ()

  assert (verifyHull2 (chans2 points)) return ()
  assert (verifyHull2 (chans2Par points)) return ()
