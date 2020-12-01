module Day01 where

import Control.Monad
import Data.List
import Data.Maybe

import AOC.Solution
import ParsingPrelude
import Util

solution :: Solution [Int] Int Int
solution = Solution
  { decodeInput = decimal `sepBy` space1
  , solveA = Solver
    { solve = productOfTwo 2020
    , display = show
    } 
  , solveB = Solver
    {
      solve = productOfThree 2020
    , display = show
    }
  , tests =
    [ "1721 979 366 299 675 1456" :=> [(PartA, "514579"), (PartB, "241861950")]
    ]
  }

productOfTwo :: Int -> [Int] -> Maybe Int
productOfTwo target xs = listToMaybe do
  n:ns <- tails xs
  let m = target - n
  m*n <$ guard (m `elem` ns)

productOfThree :: Int -> [Int] -> Maybe Int
productOfThree target xs = listToMaybe do
  n:ns <- tails xs
  let m = target - n
  Just p <- pure (productOfTwo m ns)
  pure (p*n)
  