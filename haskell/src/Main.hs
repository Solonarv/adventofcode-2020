module Main where

import AOC.Harness
import qualified Day01

solutions :: Solutions
solutions = solutionsFromList
  [ S Day01.solution
  ]

main :: IO ()
main = aocMain 2020 solutions
