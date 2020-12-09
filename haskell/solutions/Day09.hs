module Day09 where

import Control.Monad

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector

import AOC.Solution
import ParsingPrelude
import Util

solution :: Solution (Vector Word) Word (Vector Word)
solution = Solution
  { decodeInput = Vector.fromList <$> decimal `sepBy` space1
  , solveA = defSolver
    { solve = findIncongruity 25
    }
  , solveB = Solver
    { solve = findEncryptionWeakness 25
    , display = \range -> show (Vector.minimum range + Vector.maximum range)
    } 
  , tests =
    [ --"35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576"
      -- :=> [(PartA, "127")]
    ]
  }

findIncongruity :: Int -> Vector Word -> Maybe Word
findIncongruity w vec = go w
  where
    vecLen = Vector.length vec
    go !i
      | i >= vecLen = Nothing
      | otherwise = let
        prefix = Vector.slice (i-w) w vec
        thisElem = vec Vector.! i
        in if thisElem `elem` sums prefix
          then go (i+1)
          else Just thisElem

sums :: Vector Word -> [Word]
sums vec = let len = Vector.length vec in
  [ vec Vector.! i + vec Vector.! j
  | i <- [0 .. len-1]
  , j <- [i .. len-1]
  ]

findEncryptionWeakness :: Int -> Vector Word -> Maybe (Vector Word)
findEncryptionWeakness w vec = do
  target <- findIncongruity w vec
  range <- findRangeSummingTo vec target
  range <$ guard (Vector.length range > 1)

findRangeSummingTo :: Vector Word -> Word -> Maybe (Vector Word)
findRangeSummingTo vec target = go 0 2
  where
    vecLen = Vector.length vec
    go !start !len
      | start+len > vecLen = Nothing
      | otherwise = let
        subvec = Vector.slice start len vec
        sm = Vector.sum subvec
        in case sm `compare` target of
          LT -> go start (len+1)
          EQ -> Just subvec
          GT -> go (start+1) 2
