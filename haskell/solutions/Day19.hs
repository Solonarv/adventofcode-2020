{-# LANGUAGE OverloadedStrings #-}
module Day19 where

import Prelude hiding (lex)

import Data.Char
import Data.Foldable
import Data.Maybe
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP

import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import Text.Megaparsec.Char.Lexer (charLiteral)

import AOC.Solution
import ParsingPrelude
import Util

solution :: Solution (IntMap Rule, [String]) Int Int
solution = Solution
  { decodeInput = (,) <$> (IntMap.fromList <$> rule `sepEndBy` eol) <* eol <*> takeWhile1P (Just "letters") isLetter `sepBy` eol
  , solveA = defSolver
    { solve = \(rules, msgs) ->
        \case p -> countHits (matches p) msgs
        <$> toParser rules
    }
  , solveB = defSolver
    { solve = \(rules, msgs) ->
        \case p -> countHits (matches p) msgs
        <$> toParserExtra rules
    }
  , tests =
    [ unlines
      [ "0: 4 1 5"
      , "1: 2 3 | 3 2"
      , "2: 4 4 | 5 5"
      , "3: 4 5 | 5 4"
      , "4: \"a\""
      , "5: \"b\""
      , ""
      , "ababbb"
      , "bababa"
      , "abbbab"
      , "aaabbb"
      , "aaaabbb"
      ] :=> [(PartA, "2")]
    , unlines
      [ "42: 9 14 | 10 1"
      , "9: 14 27 | 1 26"
      , "10: 23 14 | 28 1"
      , "1: \"a\""
      , "11: 42 31"
      , "5: 1 14 | 15 1"
      , "19: 14 1 | 14 14"
      , "12: 24 14 | 19 1"
      , "16: 15 1 | 14 14"
      , "31: 14 17 | 1 13"
      , "6: 14 14 | 1 14"
      , "2: 1 24 | 14 4"
      , "0: 8 11"
      , "13: 14 3 | 1 12"
      , "15: 1 | 14"
      , "17: 14 2 | 1 7"
      , "23: 25 1 | 22 14"
      , "28: 16 1"
      , "4: 1 1"
      , "20: 14 14 | 1 15"
      , "3: 5 14 | 16 1"
      , "27: 1 6 | 14 18"
      , "14: \"b\""
      , "21: 14 1 | 1 14"
      , "25: 1 1 | 1 14"
      , "22: 14 14"
      , "8: 42"
      , "26: 14 22 | 1 20"
      , "18: 15 15"
      , "7: 14 5 | 1 21"
      , "24: 14 1"
      , ""
      , "abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa"
      , "bbabbbbaabaabba"
      , "babbbbaabbbbbabbbbbbaabaaabaaa"
      , "aaabbbbbbaaaabaababaabababbabaaabbababababaaa"
      , "bbbbbbbaaaabbbbaaabbabaaa"
      , "bbbababbbbaaaaaaaabbababaaababaabab"
      , "ababaaaaaabaaab"
      , "ababaaaaabbbaba"
      , "baabbaaaabbaaaababbaababb"
      , "abbbbabbbbaaaababbbbbbaaaababb"
      , "aaaaabbaabaaaaababaa"
      , "aaaabbaaaabbaaa"
      , "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa"
      , "babaaabbbaaabaababbaabababaaab"
      , "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"
      ] :=> [(PartA, "3"), (PartB, "12")]
    ]
  }

newtype Rule = Rule [Alt] deriving (Eq, Ord, Show)

newtype Alt = Alt [Piece] deriving (Eq, Ord, Show)

data Piece = Atom String | OtherRule Int deriving (Eq, Ord, Show)

lex :: Parser a -> Parser a
lex = lexeme noeol

rule :: Parser (Int, Rule)
rule = do
  i <- lex decimal
  lex ":"
  alts <- alt `sepBy` lex "|"
  pure (i, Rule alts)

alt :: Parser Alt
alt = Alt <$> many piece

piece :: Parser Piece
piece = Atom <$> lex lit <|> OtherRule <$> lex decimal
  where
    lit = "\"" *> manyTill charLiteral "\""

toParser :: IntMap Rule -> Maybe (ReadP ())
toParser rules = IntMap.lookup 0 $ löb (makeParser <$> rules)
  where
    makeParser (Rule alts) out = asum
      [ traverse_ (pieceParser out) pieces
      | Alt pieces <- alts
      ]
    pieceParser _ (Atom lit) = () <$ ReadP.string lit
    pieceParser out (OtherRule i)
      | r <- getRule i out = r

matches :: ReadP () -> String -> Bool
matches p s = not . null $ ReadP.readP_to_S (p <* ReadP.eof) s


toParserExtra :: IntMap Rule -> Maybe (ReadP ())
toParserExtra rules = IntMap.lookup 0 $ löb (IntMap.mapWithKey makeParser rules)
  where
    makeParser 8 _ out
      | r42 <- getRule 42 out = () <$ some r42
    makeParser 11 _ out
      | r31 <- getRule 31 out
      , r42 <- getRule 42 out = () <$ do l <- some r42 ; count (length l) r31
    makeParser _ (Rule alts) out = asum
      [ traverse_ (pieceParser out) pieces
      | Alt pieces <- alts
      ]
    pieceParser _ (Atom lit) = () <$ ReadP.string lit
    pieceParser out (OtherRule i)
      | r <- getRule i out = r

getRule :: Int -> IntMap (ReadP ()) -> ReadP ()
getRule i m = fromMaybe (error $ "Rule " <> show i <> "is not defined!") $ IntMap.lookup i m
