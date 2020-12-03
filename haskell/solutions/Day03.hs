module Day03 where

import Control.Arrow ((&&&))

import Control.Lens
import Data.Set (Set)
import qualified Data.Set as Set
import Linear

import AOC.Solution
import Grid2D
import ParsingPrelude
import Util

solution :: Solution (Grid2D Bool) Int Int
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

treeLocations :: Parser (Grid2D Bool)
treeLocations = fromLines <$> (some point `sepBy1` eol)
  where
    point = True <$ char '#' <|> False <$ char '.'

countTreesHit :: V2 Int -> (Grid2D Bool) -> Int
countTreesHit (V2 dx dy) grid = length
  [ ()
  | i <- [0, 1 .. (height grid - 1) `div` dy]
  , let x = (dx*i) `mod` width grid
  , let y = dy*i
  , grid ^. gridPoint x y
  ]

tryManySlopes :: [V2 Int] -> (Grid2D Bool) -> [Int]
tryManySlopes slopes grid = flip countTreesHit grid <$> slopes

slopesToTry :: [V2 Int]
slopesToTry = [V2 1 1, V2 3 1, V2 5 1, V2 7 1, V2 1 2]
