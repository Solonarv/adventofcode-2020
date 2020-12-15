{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Day15 where

import Control.Monad.ST
import Data.STRef
-- import Debug.Trace

import Control.Monad.Loops (whileM_)
-- import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as MVector
-- import qualified Data.Vector.Unboxed as Vector
-- import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap

import AOC.Solution
import ParsingPrelude
import Util

solution :: Solution [Int] Int Int
solution = Solution
  { decodeInput = decimal `sepBy` ","
  , solveA = defSolver
    { solve = Just . nthVanEck 2020
    }
  , solveB = defSolver
    { solve = Just . nthVanEck 30000000
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

-- pure implementation
-- slow and kind of messy
generateVanEckSeq :: [Int] -> Stream Int
generateVanEckSeq [] = error "empty input"
generateVanEckSeq (n:ns) = goStart IntMap.empty 1 n ns
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

-- fast implementation
-- tedious and hellish to debug
-- unboxed ST go brr
nthVanEck :: Int -> [Int] -> Int
nthVanEck n start
  | Just r <- safeIndex (n-1) start = r
  | otherwise = runST do
    seen <- MVector.new (maximum (n : start)+1)
    MVector.set seen 0
    turnR <- newSTRef 1
    lastR <- newSTRef undefined
    doStart seen turnR lastR start
    whileM_ ((< n) <$> readSTRef turnR) do
      lastVal <- readSTRef lastR
      turn <- readSTRef turnR
      lastSeen <- MVector.read seen lastVal
      MVector.write seen lastVal turn
      let out = if lastSeen == 0 then 0 else turn - lastSeen
      writeSTRef turnR $! (turn+1)
      writeSTRef lastR $! out
    readSTRef lastR
  where
    doStart _ _ _ [] = error "empty input"
    doStart _ _ lastR [x] = writeSTRef lastR $! x
    doStart seen turnR lastR (x:xs) = do
      turn <- readSTRef turnR
      MVector.write seen x turn
      writeSTRef turnR $! (turn+1)
      doStart seen turnR lastR xs
      


