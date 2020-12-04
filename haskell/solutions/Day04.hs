module Day04 where

import Control.Monad
import Data.Char (isSpace, isHexDigit)
import Data.Foldable
import Data.Maybe
-- import Debug.Trace

import Data.Map (Map)
import qualified Data.Map.Strict as Map

import AOC.Solution
import ParsingPrelude
import Util

solution :: Solution [Passport] Int Int
solution = Solution
  { decodeInput = passport `sepBy` (eol)
  , solveA = defSolver
    { solve = Just . countHits hasRequiredFields
    }
  , solveB = defSolver
    { solve = Just . countHits isValidPassport
    }
  , tests =
    [ unlines
      [ "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
      , "byr:1937 iyr:2017 cid:147 hgt:183cm"
      , ""
      , "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884"
      , "hcl:#cfa07d byr:1929"
      , ""
      , "hcl:#ae17e1 iyr:2013"
      , "eyr:2024"
      , "ecl:brn pid:760753108 byr:1931"
      , "hgt:179cm"
      , ""
      , "hcl:#cfa07d eyr:2025 pid:166559648"
      , "iyr:2011 ecl:brn hgt:59in"
      ] :=> [(PartA, "2")]
    , unlines
      [ "eyr:1972 cid:100"
      , "hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926"
      , ""
      , "iyr:2019"
      , "hcl:#602927 eyr:1967 hgt:170cm"
      , "ecl:grn pid:012533040 byr:1946"
      , ""
      , "hcl:dab227 iyr:2012"
      , "ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277"
      , ""
      , "hgt:59cm ecl:zzz"
      , "eyr:2038 hcl:74454a iyr:2023"
      , "pid:3556412378 byr:2007"
      ] :=> [(PartB, "0")]
    , unlines
      [ "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980"
      , "hcl:#623a2f"
      , ""
      , "eyr:2029 ecl:blu cid:129 byr:1989"
      , "iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"
      , ""
      , "hcl:#888785"
      , "hgt:164cm byr:2001 iyr:2015 cid:88"
      , "pid:545766238 ecl:hzl"
      , "eyr:2022"
      , ""
      , "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"
      ] :=> [(PartB, "4")]
    ]
  }

data PassportField
  = BirthYear
  | IssueYear
  | ExpirationYear
  | Height
  | HairColor
  | EyeColor
  | PassportID
  | CountryID
  deriving (Eq, Ord, Enum, Bounded, Show)

newtype Passport = Passport { fields :: Map PassportField String }
  deriving (Eq, Ord, Show)

passport :: Parser Passport
passport = Passport . Map.fromList <$> (field `sepEndBy` spaceChar)

field :: Parser (PassportField, String)
field = (,) <$> fieldName <* string ":" <*> nonSpace

fieldName :: Parser PassportField
fieldName = asum
  [ BirthYear <$ string "byr"
  , IssueYear <$ string "iyr"
  , ExpirationYear <$ string "eyr"
  , Height <$ string "hgt"
  , HairColor <$ string "hcl"
  , EyeColor <$ string "ecl"
  , PassportID <$ string "pid"
  , CountryID <$ string "cid"
  ]

nonSpace :: Parser String
nonSpace = takeWhile1P (Just "non-space characters") (not . isSpace)

hasRequiredFields :: Passport -> Bool
hasRequiredFields (Passport fields) = and
  [ f `Map.member` fields
  | f <- [BirthYear .. PassportID] -- CountryID excluded: that's the problem statement!
  ]

isValidPassport :: Passport -> Bool
isValidPassport (Passport fields) = all (applyTo fields)
  [ BirthYear      `matches` decimal >>= guard . within @Int 1920 2002
  , IssueYear      `matches` decimal >>= guard . within @Int 2010 2020
  , ExpirationYear `matches` decimal >>= guard . within @Int 2020 2030
  , Height         `matches` decimal >>= \h -> asum
    [ guard (within @Int 150 193 h) *> string "cm"
    , guard (within @Int 59 76 h) *> string "in"
    ]
  , HairColor      `matches` string "#" *> count 6 (satisfy isHexDigit)
  , EyeColor       `matches` (asum . fmap string)
    [ "amb", "blu", "brn", "gry"
    , "grn", "hzl", "oth"
    ]
  , PassportID     `matches` count 9 digitChar
  ]    

infix 0 `matches`
matches :: k -> Parser x -> (k, String -> Bool)
matches k ps = (k, isJust . parseMaybe ps)

applyTo :: Ord k => Map k v -> (k, v -> Bool) -> Bool
applyTo m (k,p) = maybe False p (Map.lookup k m)