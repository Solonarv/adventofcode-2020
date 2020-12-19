module ParsingPrelude
  ( Parser
  , chainl, noeol
  , module Text.Megaparsec
  , module Text.Megaparsec.Char
  , module Text.Megaparsec.Char.Lexer
) where

import Control.Monad
import Data.Char
import Data.Foldable
import Data.Void

import Text.Megaparsec hiding (Stream(..), State(..))
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal, binary, octal, hexadecimal, scientific, float, signed, lexeme)
import Text.Megaparsec.Stream (Token)

type Parser = Parsec Void String

chainl :: MonadPlus m => m a -> m (a -> a -> a) -> m a
chainl el op = do
  e1 <- el
  pairs <- many ((,) <$> op <*> el)
  pure (foldl' (\a (f, e) -> f a e) e1 pairs)

noeol :: (MonadParsec e s m, Token s ~ Char) => m ()
noeol = () <$ takeWhileP (Just "non-newline whitespace") (\c -> c /= '\n' && c /= '\r' && isSpace c)
