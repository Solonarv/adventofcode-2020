module AOC.Solution where

import Data.Typeable

import DynMap
import ParsingPrelude (Parser)

data Part = PartA | PartB deriving (Eq, Ord, Show)

allParts :: [Part]
allParts = [PartA, PartB]

displayPart :: Part -> Char
displayPart PartA = 'a'
displayPart PartB = 'b'

data Solution i a b = Solution
  { decodeInput :: Parser i
  , solveA      :: Solver i a
  , solveB      :: Solver i b
  , tests       :: [Test]
  }

data Solver i o = Solver
  { solve :: HasDyns => i -> Maybe o
  , display :: HasDyns => o -> String
  }

defSolver :: Show o => Solver i o
defSolver = Solver (const Nothing) show

runSolver :: HasDyns => Solution i a b -> Part -> i -> Maybe String
runSolver Solution{solveA = Solver{solve,display}} PartA dat = display <$> solve dat
runSolver Solution{solveB = Solver{solve,display}} PartB dat = display <$> solve dat

data Test
  = String :=> [(Part, String)]
  | forall a. Typeable a => WithDyn String a Test

processTest :: Test -> (DynMap, String, [(Part, String)])
processTest (i :=> o) = (emptyDynMap, i, o)
processTest (WithDyn k v inner) = (addDyn k v dyns, i, o)
  where (dyns, i, o) = processTest inner