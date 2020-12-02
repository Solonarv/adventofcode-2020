module Day02 where

import AOC.Solution
import ParsingPrelude
import Util

solution :: Solution [(Policy, String)] Int Int
solution = Solution
  { decodeInput = entry `sepBy` eol
  , solveA = defSolver
    { solve = Just . countHits (uncurry isValidOld)
    }
  , solveB = defSolver
    { solve = Just . countHits (uncurry isValidNew)
    }
  , tests =
    [ unlines [ "1-3 a: abcde"
              , "1-3 b: cdefg"
              , "2-9 c: ccccccccc"
              ] :=> [(PartA, "2"), (PartB, "1")]
    ]
  }

data Policy = Policy Int Int Char

policy :: Parser Policy
policy = Policy
  <$>  decimal
  <*  string "-"
  <*> decimal
  <*  space1
  <*> letterChar

entry :: Parser (Policy, String)
entry = (,)
  <$> policy
  <* string ":"
  <* space1
  <*> some letterChar 

isValidOld :: Policy -> String -> Bool
isValidOld (Policy minCount maxCount character) s = minCount <= occ && occ <= maxCount
  where occ = countHits (== character) s 

isValidNew :: Policy -> String -> Bool
isValidNew (Policy fstIndex sndIndex character) s = fstMatches /= sndMatches
  where
    fstMatches = safeIndex (fstIndex-1) s == Just character
    sndMatches = safeIndex (sndIndex-1) s == Just character