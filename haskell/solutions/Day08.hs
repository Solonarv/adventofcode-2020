{-# LANGUAGE OverloadedStrings #-}
module Day08 where

import Data.Foldable
import Data.Functor.Identity
import Data.Maybe
-- import Debug.Trace

import Control.Monad.Reader
import Control.Monad.State
import Data.Vector (Vector)
import qualified Data.Vector as Vector

import AOC.Solution
import ParsingPrelude
import Util

solution :: Solution (Vector Instr) Int Int
solution = Solution
  { decodeInput = Vector.fromList <$> (instr `sepBy` eol)
  , solveA = defSolver
    { solve = fst . runUntilLoop
    }
  , solveB = defSolver
    { solve = programResult <=< fixedProgram
    }
  , tests =
    [ unlines
      [ "nop +0"
      , "acc +1"
      , "jmp +4"
      , "acc +3"
      , "jmp -3"
      , "acc -99"
      , "acc +1"
      , "jmp -4"
      , "acc +6"
      ] :=> [(PartA, "5"), (PartB, "8")]
    ]
  }

data Instr = Instr { instrOp :: !Op, instrArg :: !Int }
  deriving (Eq, Ord, Show)

data Op = Acc | Jmp | Nop
  deriving (Eq, Ord, Show)

instr :: Parser Instr
instr = Instr <$> op <* space <*> signed noSpace decimal
  where noSpace = pure ()

op :: Parser Op
op = asum
  [ Acc <$ "acc"
  , Jmp <$ "jmp"
  , Nop <$ "nop"
  ]

data CPUState = CPUState
  { cpuPC :: Int
  , cpuAcc :: Int
  , cpuVisited :: Vector Bool
  }
  deriving (Eq, Ord, Show)

newtype CPU a = CPU { runCPU :: Vector Instr -> CPUState -> (a, CPUState) }
  deriving (Functor, Applicative, Monad, MonadReader (Vector Instr), MonadState CPUState)
  via ReaderT (Vector Instr) (State CPUState)

runUntilLoop :: Vector Instr -> (Maybe Int, CPUState)
runUntilLoop prog = runCPU stepUntilRepeat prog startingState
  where
    startingState = CPUState 0 0 (Vector.replicate (length prog) False)

stepUntilRepeat :: CPU (Maybe Int)
stepUntilRepeat = do
  CPUState{cpuPC, cpuAcc, cpuVisited} <- get
  if cpuPC >= 0 && cpuPC < length cpuVisited
    then if cpuVisited Vector.! cpuPC
      then pure (Just cpuAcc)
      else singleStep >> stepUntilRepeat
    else pure Nothing

singleStep :: CPU ()
singleStep = do
  pc <- gets cpuPC
  Instr op_ arg <- asks (Vector.! pc)
  modify \s -> s { cpuVisited = cpuVisited s Vector.// [(pc, True)] }
  case op_ of
    Acc -> modify \s -> s { cpuPC = pc+1, cpuAcc = cpuAcc s + arg }
    Nop -> modify \s -> s { cpuPC = pc+1 }
    Jmp -> modify \s -> s { cpuPC = pc+arg }

halts :: Vector Instr -> Bool
halts = isNothing . fst . runUntilLoop

fixedProgram :: Vector Instr -> Maybe (Vector Instr)
fixedProgram prog = case runUntilLoop prog of
  (Nothing, _) -> Just prog -- Vector Instr halts already
  (_, st) -> let
    otherOp Acc = Nothing
    otherOp Jmp = Just Nop
    otherOp Nop = Just Jmp
    in listToMaybe
      [ betterProgram
      | i <- [1 .. length prog-1]
      , cpuVisited st Vector.! i
      , let candInstr = prog Vector.! i
      , Just newOp <- [otherOp (instrOp candInstr)]
      , let betterProgram = prog Vector.// [(i, candInstr { instrOp = newOp } )]
      , halts betterProgram
      ]

programResult :: Vector Instr -> Maybe Int
programResult prog = case runUntilLoop prog of
  (Nothing, s) -> Just (cpuAcc s)
  _ -> Nothing
