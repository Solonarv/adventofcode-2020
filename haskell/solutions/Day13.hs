{-# LANGUAGE OverloadedStrings #-}
module Day13 where

import Data.Bifunctor
import Data.Functor
import Data.Maybe

import AOC.Solution
import ParsingPrelude
import Util

solution :: Solution (Integer, [Maybe Integer]) Integer Integer
solution = Solution
  { decodeInput = (,) <$> decimal <* space1 <*> busLine `sepBy` ","
  , solveA = defSolver
    { solve = fmap (uncurry (*)) . uncurry findNextBus . second catMaybes
    }
  , solveB = defSolver
    { solve = findBusCascade . snd
    }
  , tests =
    [ "939 7,13,x,x,59,x,31,19"
      :=> [(PartA, "295"), (PartB, "1068781")]
    , "0 17,x,13,19"
      :=> [(PartB, "3417")]
    , "0 67,7,59,61"
      :=> [(PartB, "754018")]
    , "0 67,x,7,59,61"
      :=> [(PartB, "779210")]
    , "0 67,7,x,59,61"
      :=> [(PartB, "1261476")]
    , "0 1789,37,47,1889"
      :=> [(PartB, "1202161486")]
    ]
  }

busLine :: Parser (Maybe Integer)
busLine = Nothing <$ "x" <|> Just <$> decimal

timeTillDeparture :: Integer -> Integer -> Integer
timeTillDeparture now freq = freq - ((now-1) `mod` freq) - 1

findNextBus :: Integer -> [Integer] -> Maybe (Integer, Integer)
findNextBus now freqs = safely (minimumOn (timeTillDeparture now)) freqs
  <&> \line -> (line, timeTillDeparture now line)

findBusCascade :: [Maybe Integer] -> Maybe Integer
findBusCascade = solveGCRT . toCongs

data Cong = Cong Integer Integer
  deriving (Eq, Ord, Show)

toCongs :: [Maybe Integer] -> [Cong]
toCongs = catMaybes . zipWith cong [0..]
  where
    cong _ Nothing = Nothing
    cong i (Just w) = Just (Cong w i)

solveGCRT :: [Cong] -> Maybe Integer
solveGCRT [] = Just 0
solveGCRT [Cong m i] = Just (negate i `mod` m)
solveGCRT (Cong m1 i1 : Cong m2 i2 : cs) = let
  l = lcm m1 m2
  (g, u, v) = egcd m1 m2
  in if i1 `mod` g == i2 `mod` g
    then solveGCRT (Cong l ((i1*v*m2 + i2*u*m1) `div` g `mod` l) : cs)
    else Nothing

egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd a b = go a b 1 0 0 1
  where go r 0  x y _  _  = (r,x,y)
        go r r' x y x' y' = go r' r'' x' y' x'' y''
          where r'' = r `rem` r'
                q   = r `div` r'
                x'' = x - q * x'
                y'' = y - q * y'