{-# LANGUAGE OverloadedStrings #-}
module Day20 where

import Data.Function
import Data.Maybe
import Data.Monoid (Alt(..))
-- import Debug.Trace

import Control.Lens
import Control.Monad.Logic
-- import Data.Vector (Vector)
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap

import AOC.Solution
import Grid2D
import ParsingPrelude
import Util

solution :: Solution [Tile] (Grid2D Tile) ()
solution = Solution
  { decodeInput = tile `sepBy` eol
  , solveA = Solver
    { solve = _
    , display = show . product . map tileId . corners
    }
  , solveB = defSolver
  , tests = [] -- no tests: too big, don't want to clutter the source file
  }

corners :: Grid2D a -> [a]
corners g@(Grid2D w h _) = [g ^!? gridPoint x y | x <- [0, w-1], y <- [0, h-1]]

data Tile = Tile
  { tileID :: Int
  , tileContents :: Grid2D Bool
  }
  deriving (Show)

tileP :: Parser Tile
tileP = Tile <$> idLine <*> content
  where
    idLine = "Tile " *> decimal <* ":\n"
    content = fromLines <$> some point `sepEndBy` eol
    point = asum [False <$ ".", True <$ "#"]

-- TODO switch from 'Grid2D (Maybe Tile)' to 'Map (V2 Int) Tile' ???

instance Eq Tile where (==) = (==) `on` tileID
instance Ord Tile where compare = compare `on` tileID

type TileSet = IntMap (Set Tile)

solve :: Grid2D (Maybe Tile) -> LogicT (State TileSet) (Grid2D Tile)
solve grid =
  case sequenceA grid of
    Just sol -> pure sol
    _ -> do
      (x,y) <- selectUnfilledTile grid
      let edges = getSurroundingEdges grid x y
      guard (edges /= emptyEdges)
      tile <- selectTileMatching edges
      guard (tileFitsAt tile grid x y)
      solve (grid & gridPoint x y .~ tile)

drawFrom :: (Foldable t, Alternative f) => t a -> f a
drawFrom = getAlt . foldMap Alt

selectUnfilledTile :: Monad m => Grid2D (Maybe a) -> LogicT m (Int, Int)
selectUnfilledTile grid@(Grid w h _) = drawFrom
  [ (x,y)
  | x <- [0 .. w-1]
  , y <- [0 .. h-1]
  , grid ^? gridPoint w h == Just Nothing
  ]

data Edges = Edges
  { topEdge, rightEdge, bottomEdge, leftEdge :: Maybe Int
  } deriving (Eq, Ord, Show)

emptyEdges :: Edges
emptyEdges = Edges Nothing Nothing Nothing Nothing

getSurroundingEdges :: Grid2D (Maybe Tile) -> Int -> Int -> Edges
getSurroundingEdges grid x y = Edges
  do grid ^? gridPoint x (y+1) . _Just . to tileBottomEdge
  do grid ^? gridPoint (x+1) y . _Just . to tileLeftEdge
  do grid ^? gridPoint x (y-1) . _Just . to tileTopEdge
  do grid ^? gridPoint (x-1) y . _Just . to tileRightEdge

selectTileMatching :: Edges -> LogicT (State TileSet) Tile
selectTileMatching edges@Edges{topEdge, rightEdge, bottomEdge, leftEdge} = do
  case catMaybes [topEdge, rightEdge, bottomEdge, leftEdge] of
    []-> fail "no edges specified"
    es -> do
      Just found <- traverse (gets . IntMap.lookup) es
      candidate <- drawFrom (foldr1 Set.intersection found)
      each . at candidate ?= Nothing
      rotatedCandidate <- allRotations candidate
      guard (rotatedCandidate `matchesEdges` edges)
      pure rotatedCandidate

allRotations :: Monad m => Tile -> LogicT m Tile
allRotations tile = properRotations tile <|> properRotations (mirrorHorizontally tile)

properRotations :: Monad m => Tile -> LogicT m Tile
properRotations (Tile tid grid) = Tile tid <$> (drawFrom [tile, rotate90 tile] >>- \rot -> drawFrom [rot, rotate180 rot])

rotate90 :: Grid2D a -> Grid2D a
rotate90 g@(Grid2D w h _) = genGrid h w \x y -> g ^!? gridPoint (h-1-y) x

rotate180 :: Grid2D a -> Grid2D a
rotate180 g@(Grid w h _) = genGrid w h \x y -> g ^!? gridPoint (w-1-x) (h-1-y)

mirrorHorizontally :: Tile -> Tile
mirrorHorizontally (Tile tid grid@(Grid w h _)) = Tile tid $ genGrid w h \x y -> grid ^!? gridPoint (w-1-x) y

tileTopEdge, tileRightEdge, tileBottomEdge, tileRightEdge :: Tile -> Int
tileTopEdge    (Tile _ grid) = bitsToBinary $ ray 0              0               1    0    grid
tileRightEdge  (Tile _ grid) = bitsToBinary $ ray (width grid-1) 0               0    0    grid
tileBottomEdge (Tile _ grid) = bitsToBinary $ ray (width grid-1) (height grid-1) (-1) 0    grid
tileLeftEdge   (Tile _ grid) = bitsToBinary $ ray 0              (height grid-1) 0    (-1) grid

matchesEdges :: Tile -> Edges -> Bool
tile `matchesEdges` edges = and
  [ tileTopEdge    tile `matches` topEdge    edges
  , tileRightEdge  tile `matches` rightEdge  edges
  , tileBottomEdge tile `matches` bottomEdge edges
  , tileLeftEdge   tile `matches` leftEdge   edges
  ]
  where
    _ `matches` Nothing = True
    x `matches` Just y = x == y