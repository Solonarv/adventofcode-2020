module Main where

import AOC.Harness
import qualified Day01
import qualified Day02

solutions :: Solutions
solutions = solutionsFromList
  [ S Day01.solution
  , S Day02.solution
  ]

main :: IO ()
main = aocMain 2020 solutions
