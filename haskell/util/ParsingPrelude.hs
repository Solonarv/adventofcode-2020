module ParsingPrelude
  ( Parser
  , module Text.Megaparsec
  , module Text.Megaparsec.Char
  , module Text.Megaparsec.Char.Lexer
) where

import Data.Void

import Text.Megaparsec hiding (Stream(..))
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal, binary, octal, hexadecimal, scientific, float, signed)

type Parser = Parsec Void String
