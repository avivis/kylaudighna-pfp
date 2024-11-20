module Lib (
  Point2D (Point2D),
  Line2D (Line2D),
) where

-- TODO: Turn point and line into classes so we can easily switch between 2D and 3D

data Point2D = Point2D Double Double deriving (Show)
instance Eq Point2D where
  (Point2D x1 y1) == (Point2D x2 y2) = x1 == x2 && y1 == y2

data Line2D = Line2D Point2D Point2D deriving (Show)
