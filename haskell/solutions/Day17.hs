{-# LANGUAGE OverloadedStrings, UndecidableInstances #-}
module Day17 where

import Data.Foldable
-- import Data.Function
-- import Debug.Trace

import Data.Set (Set)
import qualified Data.Set as Set
import Linear.V3
import Linear.V4

import AOC.Solution
import ParsingPrelude
import Util

solution :: Solution (Grid V3) (Grid V3) (Grid V4)
solution = Solution
  { decodeInput = grid
  , solveA = Solver
    { solve = Just . funcpow 6 step
    , display = ("Live cells: "<>) . show . Set.size . liveCells
    }
  , solveB = Solver
    { solve = Just . funcpow 6 step . adjoin4thDim
    , display = ("Live cells: "<>) . show . Set.size . liveCells
    }
  , tests =
    [ unlines
      [ ".#."
      , "..#"
      , "###"
      ] :=> [(PartA, "Live cells: 112"), (PartB, "Live cells: 848")]
    ]
  }

data Bounds a = Bounds !a !a deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Ord a => Semigroup (Bounds a) where
  Bounds loa hia <> Bounds lob hib = Bounds (loa `min` lob) (hia `max` hib)

instance (Ord a, Bounded a) => Monoid (Bounds a) where
  mempty = Bounds maxBound minBound

boundsEntries :: Enum a => Bounds a -> [a]
boundsEntries (Bounds lo hi) = [lo .. hi]

data Grid v = Grid
  { liveCells :: !(Set (v Int))
  , interestingCells :: !(v (Bounds Int))
  }

instance (Semigroup (v (Bounds Int)), Ord (v Int)) => Semigroup (Grid v) where
  Grid la ia <> Grid lb ib = Grid (la `Set.union` lb) (ia <> ib)

instance (Ord (v Int), Monoid (v (Bounds Int))) => Monoid (Grid v) where
  mempty = Grid mempty mempty

grid :: Parser (Grid V3)
grid = toGrid3 <$> some cell `sepBy` eol
  where
    cell = choice
      [ False <$ "."
      , True <$ "#"
      ]

toGrid3 :: [[Bool]] -> Grid V3
toGrid3 cs = fold
  [ Grid (Set.singleton loc) (boundsAround loc)
  | (y, line) <- zip [0..] cs
  , (x, cell) <- zip [0..] line
  , cell
  , let loc = V3 x y 0
  ]

boundsAround :: (Num (v a), Applicative v) => v a -> v (Bounds a)
boundsAround loc = sequenceA (Bounds (loc-1) (loc+1))

step :: (Num (v Int), Applicative v, Ord (v Int), Traversable v, Monoid (v (Bounds Int))) => Grid v -> Grid v
step (Grid live bounds) = foldMap determine cellsInRange -- & \g -> trace (showGrid g) g
  where
    cellsInRange = traverse boundsEntries bounds
    determine loc = if shouldLive then Grid (Set.singleton loc) theseBounds else mempty
      where
        theseBounds = boundsAround loc
        shouldLive = if loc `Set.member ` live
          then liveInRange == 3 || liveInRange == 4
          else liveInRange == 3
        liveInRange = countHits (`Set.member` live) (traverse boundsEntries theseBounds)

showGrid3 :: Grid V3 -> String
showGrid3 (Grid live (V3 bx by bz)) = unlines
  [ unlines $ ("z = " <> show z) :
    [ [ if V3 x y z `Set.member` live then '#' else '.'
      | x <- boundsEntries bx
      ]
    | y <- boundsEntries by
    ]
  | z <- boundsEntries bz
  ]

adjoin4thDim :: Grid V3 -> Grid V4
adjoin4thDim (Grid live (V3 bx by bz)) = Grid (Set.mapMonotonic upgradeVec live) (V4 bx by bz (Bounds (-1) 1))
  where
    upgradeVec (V3 x y z) = V4 x y z 0