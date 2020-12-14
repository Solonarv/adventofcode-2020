{-# LANGUAGE OverloadedStrings #-}
module Day14 where

import Data.Bits
import Data.Foldable
import Data.Word
import Debug.Trace

import Control.Lens
import Control.Monad.State
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import AOC.Solution
import ParsingPrelude
import Util

solution :: Solution [Instr] Val Val
solution = Solution
  { decodeInput = instr `sepBy` eol
  , solveA = defSolver
    { solve = Just . programValue processInstrV1
    }
  , solveB = defSolver
    { solve = Just . programValue processInstrV2
    }
  , tests =
    [ unlines
      [ "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
      , "mem[8] = 11"
      , "mem[7] = 101"
      , "mem[8] = 0"
      ] :=> [(PartA, "165")]
    , unlines
      [ "mask = 000000000000000000000000000000X1001X"
      , "mem[42] = 100"
      , "mask = 00000000000000000000000000000000X0XX"
      , "mem[26] = 1"
      ] :=> [(PartB, "208")]
    ]
  }

data Instr = Assign !Int !Val | SetMask Mask
  deriving (Eq, Ord, Show)

type Val = Word64 -- only bottom 36 bits are set
type Mask = [Maybe Bool]

applyValMask :: Mask -> Val -> Val
applyValMask bits v = add .|. (cut .&. v)
  where
    cut = bitsToBinary ((/= Just False) <$> bits)
    add = bitsToBinary ((== Just True) <$> bits)

instr :: Parser Instr
instr = maskP <|> assignment
  where
    maskP = do
      "mask = "
      bits <-count 36 maskBit
      pure (SetMask bits)
      where
        maskBit = choice
          [ Nothing <$ "X"
          , Just False <$ "0"
          , Just True <$ "1"
          ]

    assignment = do
      "mem["
      addr <- decimal
      "] = "
      val <- decimal
      pure (Assign addr val)

data CPU = CPU !Mask !(IntMap Val)

mask :: Lens' CPU Mask
mask f (CPU m v) = flip CPU v <$> f m

mem :: Lens' CPU (IntMap Val)
mem f (CPU m v) = CPU m <$> f v

processInstrV1 :: Monad m => Instr -> StateT CPU m ()
processInstrV1 (SetMask m) = mask .= m
processInstrV1 (Assign a v) = do
  curmask <- use mask
  mem . at a ?= applyValMask curmask v

programValue :: (Instr -> State CPU ()) -> [Instr] -> Val
programValue processInstr is = flip evalState startingState do
  traverse_ processInstr is
  use (mem . to sum')
  where
    startingState = CPU (replicate 36 Nothing) IntMap.empty

processInstrV2 :: Monad m => Instr -> StateT CPU m ()
processInstrV2 (SetMask m) = mask .= m
processInstrV2 (Assign a v) = do
  curmask <- use mask
  for_ (fluctuate curmask a) \addr -> mem . at addr ?= v

fluctuate :: Mask -> Int -> [Int]
fluctuate = go 35
  where
    go !_ [] !a = [a]
    go !i (Just False:bs) !a = go (i-1) bs a
    go !i (Just True:bs) !a = go (i-1) bs (a `setBit` i)
    go !i (Nothing:bs) !a = go (i-1) bs =<< [a `setBit` i, a `clearBit` i]
