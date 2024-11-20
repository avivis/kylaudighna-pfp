module Lib (
  Point2D (Point2D),
  Line2D (Line2D),
) where

-- TODO: Turn point and line into classes so we can easily switch between 2D and 3D

data Point2D = Point2D Double Double deriving (Show)
data Line2D = Line2D Point2D Point2D deriving (Show)

-- someFunc :: IO ()
-- someFunc = putStrLn "someFunc"