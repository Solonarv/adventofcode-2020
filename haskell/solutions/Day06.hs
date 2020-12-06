module Day06 where

import Data.Set (Set)
import qualified Data.Set as Set

import AOC.Solution
import ParsingPrelude
import Util

solution :: Solution [[Set Answer]] Int Int
solution = Solution
  { decodeInput = (answers `sepEndBy` eol) `sepBy` eol
  , solveA = defSolver
    { solve = Just . sum' . fmap (Set.size . Set.unions)
    }
  , solveB = defSolver
    { solve = Just . sum' . fmap (Set.size . foldr1 Set.intersection)
    }
  , tests =
    [  
    ]
  }

newtype Answer = Answer Char
  deriving (Eq, Ord, Show)

answers :: Parser (Set Answer)
answers = Set.fromList <$> some (Answer <$> letterChar)