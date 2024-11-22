module Lib (
  Point2D (Point2D),
  Line2D (Line2D),
) where

import Control.DeepSeq

-- TODO: Turn point and line into classes so we can easily switch between 2D and 3D

data Point2D = Point2D Double Double 
  deriving (Show, Eq) 

data Line2D = Line2D Point2D Point2D 
  deriving (Show, Eq) 

instance NFData Point2D where 
    rnf p = seq p ()

instance Ord Point2D where
  compare (Point2D x1 y1) (Point2D x2 y2) = 
    case compare x1 x2 of
      EQ -> compare y1 y2
      other -> other