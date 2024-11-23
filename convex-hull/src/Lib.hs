module Lib (
  Point2D (Point2D),
) where

import Control.DeepSeq (NFData, rnf)

-- TODO: Turn point and line into classes so we can easily switch between 2D and 3D

data Point2D = Point2D Double Double deriving (Show, Eq)

instance NFData Point2D where
  rnf p = seq p ()
