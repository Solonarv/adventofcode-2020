module Grid2D where

import Data.Maybe

import Control.Lens
import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Util

data Grid2D a = Grid2D !Int !Int !(Vector a)

width, height :: Grid2D a -> Int
width (Grid2D w _ _) = w
height (Grid2D _ h _) = h

gridPoint :: Int -> Int -> Lens' (Grid2D a) a
gridPoint x y f (Grid2D w h vec)
  | x >= w || y >= h
  = error $ "gridPoint: " <> show (x,y) <> " out of range, size is " <> show (w,h)
  | otherwise
  = let i = y*w + x
    in f (vec Vector.! i) <&> \a -> Grid2D w h (vec Vector.// [(i, a)])

gridPointWrap :: Int -> Int -> Lens' (Grid2D a) a
gridPointWrap x y f (Grid2D w h vec) = let
  i = (y `mod` h)*w + x `mod` w
  in f (vec Vector.! i) <&> \a -> Grid2D w h (vec Vector.// [(i, a)])

fromLines :: [[a]] -> Grid2D a
fromLines xss = let
  h = length xss
  w = sum' (length <$> listToMaybe xss)
  vec = Vector.fromList (concat xss)
  in Grid2D w h vec