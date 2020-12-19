{-# LANGUAGE OverloadedStrings #-}
module Day18 where

-- import Debug.Trace

import AOC.Solution
import ParsingPrelude
import Util

solution :: Solution [[Token]] Int Int
solution = Solution
  { decodeInput = some tok `sepBy` eol
  , solveA = defSolver
    { solve = fmap (sum' . fmap eval) . traverse (parseMaybe flatExpr)
    }
  , solveB = defSolver
    { solve = fmap (sum' . fmap eval) . traverse (parseMaybe precExpr)
    }
  , tests =
    [ "1 + 2 * 3 + 4 * 5 + 6" :=> [(PartA, "71"), (PartB, "213")]
    , "1 + (2 * 3) + (4 * (5 + 6))" :=> [(PartA, "51"), (PartB, "51")]
    , "2 * 3 + (4 * 5)" :=> [(PartA, "26"), (PartB, "46")]
    , "5 + (8 * 3 + 9 + 3 * 4 * 3)" :=> [(PartA, "437"), (PartB, "1445")]
    , "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" :=> [(PartA, "12240"), (PartB, "669060")]
    , "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" :=> [(PartA, "13632"), (PartB, "23340")]
    ]
  }

data Token = TAdd | TMul | TOpenParen | TCloseParen | TLit Int
  deriving (Eq, Ord, Show)

tok :: Parser Token
tok = lexeme noeol $ choice
  [ TAdd <$ "+"
  , TMul <$ "*"
  , TOpenParen <$ "("
  , TCloseParen <$ ")"
  , TLit <$> decimal
  ]

data Expr = Lit Int | Add Expr Expr | Mul Expr Expr
  deriving (Eq, Ord, Show)

flatExpr :: Parsec () [Token] Expr
flatExpr = atom flatExpr `chainl` op
  where
    op = choice
      [ Add <$ single TAdd
      , Mul <$ single TMul
      ]

eval :: Expr -> Int
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

atom :: Parsec () [Token] Expr -> Parsec () [Token] Expr
atom expr = choice
      [ token
          \case TLit x -> Just (Lit x); _ -> Nothing
          mempty
      , between (single TOpenParen) (single TCloseParen) expr
      ]

precExpr :: Parsec () [Token] Expr
precExpr = term `chainl` (Mul <$ single TMul)
  where
    term = atom precExpr `chainl` (Add <$ single TAdd)