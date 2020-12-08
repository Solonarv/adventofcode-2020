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
    { solve = resultIf Loop . run
    }
  , solveB = defSolver
    { solve = resultIf Halt . run <=< makeHalting
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

findLoopAcc :: Vector Instr -> Maybe Int
findLoopAcc prog = case run prog of
  (Halt, st) -> Just (cpuAcc st)
  _ -> Nothing

run :: Vector Instr -> (EndState, CPUState)
run prog = runCPU stepUntilRepeatOrHalt prog startingState
  where
    startingState = CPUState 0 0 (Vector.replicate (length prog) False)

data EndState = Halt | Loop
  deriving (Eq, Ord, Show)

resultIf :: EndState -> (EndState, CPUState) -> Maybe Int
resultIf des (got, cpu) = cpuAcc cpu <$ guard (des == got)

stepUntilRepeatOrHalt :: CPU EndState
stepUntilRepeatOrHalt = do
  CPUState{cpuPC, cpuVisited} <- get
  if cpuPC >= 0 && cpuPC < length cpuVisited
    then if cpuVisited Vector.! cpuPC
      then pure Loop
      else singleStep >> stepUntilRepeatOrHalt
    else pure Halt

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
halts = (== Halt) . fst . run

makeHalting :: Vector Instr -> Maybe (Vector Instr)
makeHalting prog = case run prog of
  (Halt, _) -> Just prog -- Vector Instr halts already
  (Loop, st) -> let
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
