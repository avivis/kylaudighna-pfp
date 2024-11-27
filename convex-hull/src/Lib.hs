module Lib (
) where

-- import Control.DeepSeq (NFData, rnf)

-- -- TODO: Turn point and line into classes so we can easily switch between 2D and 3D

-- -- ! means evaluate to WHNF, we want that for Vectors
-- data Point2D = Point2D Double Double deriving (Eq)

-- instance Show Point2D where
--   show (Point2D x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

-- instance NFData Point2D where
--   rnf p = seq p ()

-- crossProduct2D :: Point2D -> Point2D -> Point2D -> Double
-- crossProduct2D (Point2D x0 y0) (Point2D x1 y1) (Point2D x2 y2) = (x0 - x2) * (y1 - y2) - (y0 - y2) * (x1 - x2)

-- squareDist2D :: Point2D -> Point2D -> Double
-- squareDist2D (Point2D x0 y0) (Point2D x1 y1) = (x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0)
