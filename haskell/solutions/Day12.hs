{-# LANGUAGE OverloadedStrings #-}
module Day12 where

-- import Debug.Trace

import Linear.V2
import Linear.Vector

import AOC.Solution
import ParsingPrelude
import Util

solution :: Solution [Instr] (V2 Int) (V2 Int)
solution = Solution
  { decodeInput = instr `sepBy` space1
  , solveA = Solver
    { solve = Just . followInstrs (V2 0 0) (V2 1 0)
    , display = show . manhattan
    }
  , solveB = Solver
    { solve = Just . followWaypoint (V2 0 0) (V2 10 1)
    , display = show . manhattan
    }
  , tests =
    [ "F10 N3 F7 R90 F11"
      :=> [(PartA, "25"), (PartB, "286")]
    ]
  }

data Op = TurnL | TurnR | Fwd | North | South | West | East
  deriving (Eq, Ord, Show)

data Instr = Instr !Op !Int

op :: Parser Op
op = choice
  [ TurnL <$ "L"
  , TurnR <$ "R"
  , Fwd <$ "F"
  , North <$ "N"
  , South <$ "S"
  , West <$ "W"
  , East <$ "E"
  ]

instr :: Parser Instr
instr = Instr <$> op <*> decimal

followInstrs :: V2 Int -> V2 Int -> [Instr] -> V2 Int
followInstrs pos _   [] = pos
followInstrs pos dir (Instr o n:is) = case o of
  TurnL -> followInstrs pos              (rotate   n  dir)  is
  TurnR -> followInstrs pos              (rotate (-n) dir)  is
  Fwd   -> followInstrs (pos + dir ^* n) dir                is
  North -> followInstrs (pos + V2 0 n)   dir                is
  South -> followInstrs (pos - V2 0 n)   dir                is
  East  -> followInstrs (pos + V2 n 0)   dir                is
  West  -> followInstrs (pos - V2 n 0)   dir                is

followWaypoint :: V2 Int -> V2 Int -> [Instr] -> V2 Int
followWaypoint pos _  [] = pos
followWaypoint pos wp (Instr o n:is) = case o of
  TurnL -> followWaypoint pos             (rotate n wp)    is
  TurnR -> followWaypoint pos             (rotate (-n) wp) is
  Fwd   -> followWaypoint (pos + wp ^* n) wp               is
  North -> followWaypoint pos             (wp + V2 0 n)    is
  South -> followWaypoint pos             (wp - V2 0 n)    is
  East  -> followWaypoint pos             (wp + V2 n 0)    is
  West  -> followWaypoint pos             (wp - V2 n 0)    is

rotate :: Num a => Int -> V2 a -> V2 a
rotate n
  | n < 0 = funcpow (-(n `div` 90)) (negate . perp)
  | otherwise = funcpow (n `div` 90) perp

manhattan :: Num a => V2 a -> a
manhattan = sum' . fmap abs