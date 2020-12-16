{-# LANGUAGE OverloadedStrings #-}
module Day16 where

import Data.Char
import Data.List
import Data.Maybe

import Data.Map (Map)
import qualified Data.Map.Lazy as Map
import Data.Set (Set)
import qualified Data.Set as Set

import AOC.Solution
import DynMap
import ParsingPrelude
import Util

solution :: Solution (Map String [(Int,Int)], Ticket, [Ticket]) Int [Int]
solution = Solution
  { decodeInput = (,,)
      <$> (Map.fromList <$> rule `sepEndBy` eol)
      <*> ("\nyour ticket:\n" *> ticket)
      <*> ("\n\nnearby tickets:\n" *> ticket `sepBy` eol)
  , solveA = defSolver
    { solve = Just . sum' . \(rules, _, nearbys) -> concatMap (invalidFields rules) nearbys
    }
  , solveB = Solver
    { solve = findMyDeparture
    , display = \xs -> "product " <> show xs <> " = " <> show (product xs)
    }
  , tests =
    [ unlines
      [ "class: 1-3 or 5-7"
      , "row: 6-11 or 33-44"
      , "seat: 13-40 or 45-50"
      , ""
      , "your ticket:"
      , "7,1,14"
      , ""
      , "nearby tickets:"
      , "7,3,47"
      , "40,4,50"
      , "55,2,20"
      , "38,6,12"
      ] :=> [(PartA, "71")]
    , WithDyn "is-my-field" ((==) @String "seat") $
      unlines
      [ "class: 0-1 or 4-19"
      , "row: 0-5 or 8-19"
      , "seat: 0-13 or 16-19"
      , ""
      , "your ticket:"
      , "11,12,13"
      , ""
      , "nearby tickets:"
      , "3,9,18"
      , "15,1,5"
      , "5,14,9"
      ] :=> [(PartB, "product [13] = 13")]
    ]
  }

newtype Ticket = Ticket [Int]
  deriving (Eq, Ord, Show)

rule :: Parser (String, [(Int, Int)])
rule = (,) <$> fieldName <*> interval `sepBy` " or "
  where
    word = takeWhile1P (Just "letter") isLetter
    fieldName = mconcat <$> word `sepBy` space1 <* ": "
    interval = (,) <$> decimal <* "-" <*> decimal

ticket :: Parser Ticket
ticket = Ticket <$> decimal `sepBy` ","

invalidFields :: Map String [(Int,Int)] -> Ticket -> [Int]
invalidFields rules (Ticket fields) = filter (not . isValidField rules) fields

isValidField :: Map String [(Int,Int)] -> Int -> Bool
isValidField rules fld = any (fld `matchesRule`) rules

matchesRule :: Int -> [(Int, Int)] -> Bool
matchesRule fld intervals = or
  [ lo <= fld && fld <= hi
  | (lo,hi) <- intervals
  ]

isValidTicket :: Map String [(Int,Int)] -> Ticket -> Bool
isValidTicket rules = null . invalidFields rules

possibleFields :: Map String [(Int, Int)] -> Int -> Set String
possibleFields rules fld = Map.keysSet $ Map.filter (matchesRule fld) rules

guessedFields :: Map String [(Int, Int)] -> Ticket -> [Set String]
guessedFields rules (Ticket fields) = possibleFields rules <$> fields

guessedFieldsAll :: Map String [(Int, Int)] -> [Ticket] -> [Set String]
guessedFieldsAll rules tickets = foldr1 (zipWith Set.intersection) (guessedFields rules <$> filter (isValidTicket rules) tickets)

-- Note: this fails on some inputs, but fortunately those don't appear to exist
uniqueGuesses :: Ord a => [Set a] -> Maybe [a]
uniqueGuesses = traverse singletonSet . fixIterate step
  where
    step :: Ord a => [Set a] -> [Set a]
    step = processSingletons . processUniques

    processSingletons guesses = map removeSingletonEntries guesses
      where
        singletons = Set.fromList (mapMaybe singletonSet guesses)
        removeSingletonEntries st 
          | Set.size st > 1 = st Set.\\ singletons
          | otherwise = st
    processUniques guesses = map collapseUnique guesses
      where
        uniques = fst $ foldr (\st (uni, seen) ->
          let
            newUnseen = st Set.\\ seen
            newSeen = st `Set.intersection` seen
          in ( (uni Set.\\ newSeen) `Set.union` newUnseen
          , seen `Set.union` st
          )) (Set.empty, Set.empty) guesses
        collapseUnique st = case singletonSet (st `Set.intersection` uniques) of
          Nothing -> st
          Just x -> Set.singleton x
    
    singletonSet st
      | Set.size st == 1
      , Just (x, _) <- Set.minView st = Just x
      | otherwise = Nothing

myFields :: (String -> Bool) -> Ticket -> [String] -> [Int]
myFields isMyField (Ticket fields) fieldNames =
  [ fld
  | (fld, name) <- zip fields fieldNames
  , isMyField name
  ]

findMyDeparture :: HasDyns => (Map String [(Int, Int)], Ticket, [Ticket]) -> Maybe [Int]
findMyDeparture (rules, myTicket, nearbyTickets) = myFields isMyField myTicket <$> mfieldNames
  where
    mfieldNames = uniqueGuesses (guessedFieldsAll rules nearbyTickets)
    isMyField = getDyn "is-my-field" ("departure" `isPrefixOf`)
