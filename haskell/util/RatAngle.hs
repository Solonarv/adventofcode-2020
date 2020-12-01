module RatAngle where

import Data.Ratio

import Data.Finite
import Linear.V2

data RatAngle int = RatAngle (Finite 4) (Ratio int)
  deriving (Eq, Ord, Show)

atan2r :: Integral int => V2 int -> RatAngle int
atan2r (V2 x y)
  | x > 0, y >= 0
  = RatAngle 0 (y % x)
  | x <= 0, y > 0
  = RatAngle 1 (-x % y)
  | x < 0, y <= 0
  = RatAngle 2 (y % x)
  | x >= 0, y < 0
  = RatAngle 3 (-x % y)
  | otherwise
  = error "atan2r: input must be nonzero"