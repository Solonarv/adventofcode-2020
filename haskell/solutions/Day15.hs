{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Day15 where

import Debug.Trace

import Control.Lens
import Control.Monad.State
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap

import AOC.Solution
import ParsingPrelude
import Util

data Memory = Memory
  { _memStart :: [Int]
  , _memLastNumber :: Int
  , _memNow :: Int
  , _memSeen :: IntMap Int
  }
makeLenses ''Memory

solution :: Solution [Int] Int Int
solution = Solution
  { decodeInput = decimal `sepBy` ","
  , solveA = defSolver
    { solve = Just . streamAt (2020-1) . generateVanEckSeq -- 0-baseed vs. 1-based indexing, grr!
    }
  , solveB = defSolver
    { solve = Just . streamAt (30000000-1) . generateVanEckSeq
    }
  , tests =
    [ "0,3,6" :=> [(PartA, "436"), (PartB, "175594")]
    , "1,3,2" :=> [(PartA, "1"), (PartB, "2578")]
    , "2,1,3" :=> [(PartA, "10"), (PartB, "3544142")]
    , "1,2,3" :=> [(PartA, "27"), (PartB, "261214")]
    , "2,3,1" :=> [(PartA, "78"), (PartB, "6895259")]
    , "3,2,1" :=> [(PartA, "438"), (PartB, "18")]
    , "3,1,2" :=> [(PartA, "1836"), (PartB, "362")]
    ]
  }

generateVanEckSeq :: [Int] -> Stream Int
generateVanEckSeq (x:xs) = goStart IntMap.empty 1 x xs
  where
    goStart _ _ _ [] = error "empty input"
    goStart !seen !turn !lastNumber [x] = lastNumber :>> go (IntMap.insert lastNumber turn seen) (turn+1) x
    goStart !seen !turn !lastNumber (x:xs) = lastNumber :>> goStart (IntMap.insert lastNumber turn seen) (turn+1) x xs
    go !seen !turn !lastNumber = lastNumber :>> let
      next = case IntMap.lookup lastNumber seen of
        Nothing -> 0
        Just prev -> turn - prev
      seen' = IntMap.insert lastNumber turn seen
      in go seen' (turn+1) next
