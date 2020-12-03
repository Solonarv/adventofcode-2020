module Day03 where

import Control.Arrow ((&&&))

import Data.Set (Set)
import qualified Data.Set as Set
import Linear

import AOC.Solution
import ParsingPrelude
import Util

solution :: Solution (V2 Int, Set (V2 Int)) Int Int
solution = Solution
  { decodeInput = treeLocations
  , solveA = defSolver
    { solve = Just . countTreesHit (V2 3 1)
    }
  , solveB = defSolver
    { solve = Just . product . tryManySlopes slopesToTry
    }
  , tests =
    [ unlines
      [ "..##......."
      , "#...#...#.."
      , ".#....#..#."
      , "..#.#...#.#"
      , ".#...##..#."
      , "..#.##....."
      , ".#.#.#....#"
      , ".#........#"
      , "#.##...#..."
      , "#...##....#"
      , ".#..#...#.#"
      ] :=> [(PartA, "7"), (PartB, "336")]
    ]
  }

treeLocations :: Parser (V2 Int, Set (V2 Int))
treeLocations = munge <$> rawGrid
  where
    munge grid = (V2 (length (head grid)) (length grid), toSet grid)
    toSet grid = Set.fromList
      [ V2 x y
      | (y, line) <- zip [0..] grid
      , (x, tree) <- zip [0..] line
      , tree
      ]
    rawGrid = some point `sepBy1` eol
    point = True <$ char '#' <|> False <$ char '.'

countTreesHit :: V2 Int -> (V2 Int, Set (V2 Int)) -> Int
countTreesHit (V2 dx dy) (V2 width height, trees) = Set.size (trees `Set.intersection` (Set.fromList locations))
  where
    locations = [V2 (dx*i `mod` width) (dy*i) | i <- [0.. height `div` dy]]

tryManySlopes :: [V2 Int] -> (V2 Int, Set (V2 Int)) -> [Int]
tryManySlopes slopes grid = flip countTreesHit grid <$> slopes

slopesToTry :: [V2 Int]
slopesToTry = [V2 1 1, V2 3 1, V2 5 1, V2 7 1, V2 1 2]
