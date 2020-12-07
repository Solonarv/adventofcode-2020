module Day07 where

import Data.Foldable

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import AOC.Solution
import ParsingPrelude
import Util

solution :: Solution [(String, [(Int, String)])] Int Int
solution = Solution
  { decodeInput = rule `sepBy` eol
  , solveA = defSolver
    -- subtract 1 because the starting node is contained in the returned set
    -- this is a slightly hacky solution and doesn't properly account for potential cycles
    -- but the input is probably a DAG anyway
    { solve = Just . subtract 1 . Set.size . successors "shiny gold" . reverseMapping
    }
  , solveB = defSolver
    { solve = Just . totalChildCount "shiny gold" . Map.fromListWith (<>)
    }
  , tests =
    [ "light red bags contain 1 bright white bag, 2 muted yellow bags.\n\
      \dark orange bags contain 3 bright white bags, 4 muted yellow bags.\n\
      \bright white bags contain 1 shiny gold bag.\n\
      \muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\n\
      \shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\n\
      \dark olive bags contain 3 faded blue bags, 4 dotted black bags.\n\
      \vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\n\
      \faded blue bags contain no other bags.\n\
      \dotted black bags contain no other bags."
      :=> [(PartA, "4"), (PartB, "32")]
    , "shiny gold bags contain 2 dark red bags.\n\
      \dark red bags contain 2 dark orange bags.\n\
      \dark orange bags contain 2 dark yellow bags.\n\
      \dark yellow bags contain 2 dark green bags.\n\
      \dark green bags contain 2 dark blue bags.\n\
      \dark blue bags contain 2 dark violet bags.\n\
      \dark violet bags contain no other bags."
      :=> [(PartB, "126")]
    ]
  }

rule :: Parser (String, [(Int, String)])
rule = do
  outer <- coloredBag
  string " contain "
  (,) outer <$> asum
    [ [] <$ string "no other bags"
    , do (,) <$> decimal <* space <*> coloredBag
      `sepBy` string ", "
    ] <* string "."

coloredBag :: Parser String
coloredBag = some letterChar <> string " " <> some letterChar <* string " bag" <* optional (string "s")

reverseMapping :: Ord v => [(v, [(n, v)])] -> Map v [v]
reverseMapping xs = Map.fromListWith (<>)
  [ (inner, [outer])
  | (outer, inners) <- xs
  , (_, inner) <- inners
  ]

successors :: Ord v => v -> Map v [v] -> Set v
successors start gr = go start Set.empty
  where
    go node seen
      | node `Set.member` seen = seen
      | otherwise
      , seen' <- Set.insert node seen = case Map.lookup node gr of
          Nothing -> seen'
          Just children -> foldr go seen' children

-- Count how many bags must be inside the outer bag
-- careful! will loop forever if it reaches a cycle
-- in the input graph!
totalChildCount :: Ord v => v -> Map v [(Int, v)] -> Int
totalChildCount start gr = go start - 1
  where
    go node = case Map.lookup node gr of
      Nothing -> 0
      Just children -> 1 + sum' [n * go inner | (n, inner) <- children]