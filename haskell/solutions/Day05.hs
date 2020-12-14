module Day05 where

import Control.Monad
import Data.Bits
import Data.Maybe
import Data.List
import Data.Word

import AOC.Solution
import ParsingPrelude hiding (binary)
import Util

solution :: Solution [Word16] Word16 [Word16]
solution = Solution
  { decodeInput = (seatID <$> seat) `sepBy` space1
  , solveA = defSolver
    { solve = safely maximum
    }
  , solveB = defSolver
    { solve = Just . findGaps
    }
  , tests =
    [ "BFFFBBFRRR" :=> [(PartA, "567")]
    , "BFFFBBFRRR FFFBBBFRRR BBFFBBFRLL" :=> [(PartA, "820")]
    ]
  }

data Seat = Seat { row :: Word8, col :: Word8 }
  deriving (Eq, Ord, Show)

seat :: Parser Seat
seat = Seat <$> binary "F" "B" 7 <*> binary "L" "R" 3

binary :: Bits a => String -> String -> Int -> Parser a
binary s0 s1 n = bitsToBinary <$> count n (False <$ string s0 <|> True <$ string s1)

seatID :: Seat -> Word16
seatID (Seat r c) = fromIntegral r * 8 + fromIntegral c

findGaps :: [Word16] -> [Word16]
findGaps (sort -> xs) = mapMaybe gapCenter (zip xs (tail xs))
  where
    gapCenter :: (Word16, Word16) -> Maybe Word16
    gapCenter (p,q) = (p+1) <$ guard (p+2 == q)
