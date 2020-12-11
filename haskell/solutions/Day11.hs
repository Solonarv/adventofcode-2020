{-# LANGUAGE OverloadedStrings #-}
module Day11 where

import Data.Foldable

import Control.Lens

import AOC.Solution
import Grid2D
import ParsingPrelude
import Util

solution :: Solution (Grid2D Tile) Int Int
solution = Solution
  { decodeInput = fromLines <$> some tile `sepBy` eol
  , solveA = defSolver
    { solve = fmap (countHits (== OccupiedSeat)) . findStableLayoutWith stepAdjacent
    }
  , solveB = defSolver
    { solve = fmap (countHits (== OccupiedSeat)) . findStableLayoutWith stepRays
    }
  , tests =
    [ unlines
      [ "L.LL.LL.LL"
      , "LLLLLLL.LL"
      , "L.L.L..L.."
      , "LLLL.LL.LL"
      , "L.LL.LL.LL"
      , "L.LLLLL.LL"
      , "..L.L....."
      , "LLLLLLLLLL"
      , "L.LLLLLL.L"
      , "L.LLLLL.LL"
      ] :=> [(PartA, "37"), (PartB, "26")]
    ]
  }

data Tile = Floor | EmptySeat | OccupiedSeat
  deriving (Eq, Ord, Show)

tile :: Parser Tile
tile = asum
  [ Floor <$ "."
  , EmptySeat <$ "L"
  , OccupiedSeat <$ "#"
  ]

tileChar :: Tile -> Char
tileChar = \case
  Floor -> '.'
  EmptySeat -> 'L'
  OccupiedSeat -> '#'

stepAdjacent :: Grid2D Tile -> Grid2D Tile
stepAdjacent grid = genGrid (width grid) (height grid) \x y ->
  let adjacentOccupiedSeats = countHits (== OccupiedSeat) (adjacents x y grid)
  in case grid ^?! gridPoint x y of
    Floor -> Floor
    EmptySeat
      | adjacentOccupiedSeats == 0
      -> OccupiedSeat
    OccupiedSeat
      | adjacentOccupiedSeats >= 4
      -> EmptySeat
    t -> t

findStableLayoutWith :: (Grid2D Tile -> Grid2D Tile) -> Grid2D Tile -> Maybe (Grid2D Tile)
findStableLayoutWith step grid = case iterateUntilAnyRepeat step grid of
  (1, fixpoint) -> Just fixpoint
  _ -> Nothing

stepRays :: Grid2D Tile -> Grid2D Tile
stepRays grid = genGrid (width grid) (height grid) \x y ->
  let
    rayOccupied (drop 1 -> ts) = case dropWhile (== Floor) ts of
      (OccupiedSeat:_) -> True
      _ -> False
    occupiedSeats = countHits rayOccupied
      [ ray x y dx dy grid
      | dx <- [-1, 0, 1]
      , dy <- [-1, 0, 1]
      , dx /= 0 || dy /= 0
      ]
  in case grid ^?! gridPoint x y of
    Floor -> Floor
    EmptySeat
      | occupiedSeats == 0
      -> OccupiedSeat
    OccupiedSeat
      | occupiedSeats >= 5
      -> EmptySeat
    t -> t
