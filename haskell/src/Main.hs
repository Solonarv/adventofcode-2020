module Main where

import AOC.Harness
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15

solutions :: Solutions
solutions = solutionsFromList
  [ S Day01.solution
  , S Day02.solution
  , S Day03.solution
  , S Day04.solution
  , S Day05.solution
  , S Day06.solution
  , S Day07.solution
  , S Day08.solution
  , S Day09.solution
  , S Day10.solution
  , S Day11.solution
  , S Day12.solution
  , S Day13.solution
  , S Day14.solution
  , S Day15.solution
  ]

main :: IO ()
main = aocMain 2020 solutions

