module AOC.Solution where

import Data.Void (Void)

import Text.Megaparsec (Parsec)

data Part = PartA | PartB deriving (Eq, Ord, Show)

allParts :: [Part]
allParts = [PartA, PartB]

displayPart :: Part -> Char
displayPart PartA = 'a'
displayPart PartB = 'b'

data Solution i a b = Solution
  { decodeInput :: Parsec Void String i
  , solveA      :: Solver i a
  , solveB      :: Solver i b
  , tests       :: [Test]
  }

data Solver i o = Solver
  { solve :: i -> Maybe o
  , display :: o -> String
  }

defSolver :: Show o => Solver i o
defSolver = Solver (const Nothing) show

runSolver :: Solution i a b -> Part -> i -> Maybe String
runSolver Solution{solveA = Solver{solve,display}} PartA dat = display <$> solve dat
runSolver Solution{solveB = Solver{solve,display}} PartB dat = display <$> solve dat

data Test = String :=> [(Part, String)]