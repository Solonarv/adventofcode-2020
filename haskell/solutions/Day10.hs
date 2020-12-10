module Day10 where

import Data.List
import Data.Monoid

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import qualified Data.IntMap as IntMap

import AOC.Solution
import ParsingPrelude

solution :: Solution [Int] Int Int
solution = Solution
  { decodeInput = decimal `sepBy` space1
  , solveA = defSolver
    { solve = Just . combine . countDifferences . addEnds
    }
  , solveB = defSolver
    { solve = Just . countPaths
    }
  , tests =
    [ "16 10 15 5 1 11 7 19 6 12 4"
      :=> [(PartA, "35"), (PartB, "8")]
    , "28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11 1 32 25 35 8 17 7 9 4 2 34 10 3"
      :=> [(PartA, "220"), (PartB, "19208")]
    ]
  }

combine :: (Sum Int, Sum Int) -> Int
combine (x, z) = getSum (x*z)

addEnds :: [Int] -> [Int]
addEnds xs = 0 : maximum xs+3 : xs

countDifferences :: [Int] -> (Sum Int, Sum Int)
countDifferences = summarize . (zipWith subtract <*> tail) . sort
  where
    summarize = foldMap \case
      1 -> (1, 0)
      3 -> (0, 1)
      _ -> mempty

countPaths :: [Int] -> Int
countPaths xs = countDagPaths 0 mx (mkDag (0:mx:xs))
  where mx = maximum xs

mkDag :: [Int] -> IntSet
mkDag = IntSet.fromList

countDagPaths :: Int -> Int -> IntSet -> Int
countDagPaths start end dag = paths start
  where
    paths node 
      | node == end = 1
      | otherwise = IntMap.findWithDefault 0 node (IntMap.fromSet go dag)
    go node = paths (node+1) + paths (node+2) + paths (node+3)