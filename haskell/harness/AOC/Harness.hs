{-# LANGUAGE OverloadedStrings #-}
module AOC.Harness where

import Control.Exception
import Control.Monad
import Data.Char (isSpace)
import Data.Foldable
import qualified Data.List as List
import Data.Proxy
import Data.Traversable
import GHC.TypeLits
import System.Exit (die)
import System.IO
import Text.Printf
import Text.Read

import qualified Data.ByteString as ByteString
import Data.Finite
import Data.Finite.Internal -- We need this to define a PrintfArg (Finite n) instance
import qualified Data.HashMap.Lazy as HashMap
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
-- import Data.Time.Clock
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Network.HTTP.Req as Req
-- import qualified Network.HTTP.Client as HTTP
import Options.Applicative
import qualified System.Console.ANSI as Ansi
import System.Directory
import Text.Megaparsec (parse, errorBundlePretty, eof)
import qualified Text.Toml as Toml
import qualified Text.Toml.Types as Toml

import AOC.Solution
import DynMap

type Day = Finite 25

data RunTarget = RunAll
               | RunSolution Day
               | RunSolutionPart Day [Part]

data Opts = O
  { oCfg          :: FilePath
  , oInputDataDir :: FilePath
  }

data Command = CmdFetchInput Opts Day
             | CmdTest       RunTarget
             | CmdSolve Opts RunTarget Bool

parseOpts :: Parser Opts
parseOpts = O
  <$> strOption 
    (  long "cfg"
    <> value "aoc.toml"
    <> showDefaultWith id
    <> metavar "CONFIG_FILE"
    <> help "Configuration file (TOML) to read session token from."
    )
  <*> strOption
    (  long "input"
    <> short 'i'
    <> value "input"
    <> showDefaultWith id
    <> metavar "DIR"
    <> help "Directory where input data is stored."
    )

parseDay :: Parser Day
parseDay = argument readMFinite
  (  metavar "DAY"
  <> help "Which day's challenge to fetch or run the solution for. Omit to run all solutions."
  )

parsePart :: Parser [Part]
parsePart = argument readMParts
  (  metavar "PART"
  <> help "Which part of a challenge to run. Omit to run all parts."
  )

parseRunTarget :: Parser RunTarget
parseRunTarget = runTarget <$> optional parseDay <*> optional parsePart
  where
    runTarget Nothing _ = RunAll
    runTarget (Just day) Nothing = RunSolution day
    runTarget (Just day) (Just part) = RunSolutionPart day part

parseCommand :: Parser Command
parseCommand =
  hsubparser
    (  command "fetch" 
        (info fetchCmd (progDesc "Fetch the input for a given day."))
    <> command "test"
        (info testCmd (progDesc "Test one or more solution(s) using the challenge's example inputs."))
    <> command "run"
        (info runCmd (progDesc "Run one or more solution(s) on the actual input."))
    )
  where
    fetchCmd = CmdFetchInput <$> parseOpts <*> parseDay
    testCmd = CmdTest <$> parseRunTarget
    runCmd = CmdSolve
      <$> parseOpts
      <*> parseRunTarget
      <*> switch
        (  long "submit"
        <> help "Automatically submit the answer."
        )

data Cfg = Cfg
  { cfgToken :: Maybe Text
  } deriving Show

emptyCfg :: Cfg
emptyCfg = Cfg Nothing

fgColor :: Ansi.ColorIntensity -> Ansi.Color -> IO ()
fgColor intensity color = Ansi.setSGR [Ansi.SetColor Ansi.Foreground intensity color]

parseCfgFile :: FilePath -> IO Cfg
parseCfgFile fp = handle handler $ do
    parseResult <- Toml.parseTomlDoc fp <$> Text.readFile fp
    case parseResult of
      Left err -> do
        fgColor Ansi.Dull Ansi.Red
        printf "Error parsing %v as TOML:\n" fp
        print err
        useEmptyCfg      
      Right tbl -> case HashMap.lookup "session-token" tbl of
        Nothing -> pure $ Cfg Nothing
        Just (Toml.VString tok) -> pure $ Cfg (Just tok)
        _ -> do
          fgColor Ansi.Dull Ansi.Red
          printf "Illegal TOML format: `session-token` should be a string.\n"
          useEmptyCfg
  where
    handler (e :: IOException) = do
      fgColor Ansi.Dull Ansi.Red
      printf "Error opening %v:\n" fp
      print e
      useEmptyCfg
    useEmptyCfg = do
      printf "Using default config: %v\n" (show emptyCfg)
      pure emptyCfg


aocMain :: {- | The year we're in -} Int -> {- | The solutions -} Solutions -> IO ()
aocMain yr solutions = do
  hSetEncoding stdout utf8
  cmd <- execParser $ info (parseCommand <**> helper) (progDesc $ "Advent of Code " <> show yr <> " solutions.")
  case cmd of
    CmdFetchInput opts day      -> fetchInput yr opts day
    CmdTest target              -> runTest target solutions
    CmdSolve opts target upload -> runSolve opts target upload solutions


data ASolution = forall i a b. S (Solution i a b)

type Solutions = Vector ASolution

solutionsFromList :: [ASolution] -> Solutions
solutionsFromList = Vector.fromList

solutionForDay :: Solutions -> Day -> Maybe ASolution
solutionForDay solutions day = solutions Vector.!? (fromIntegral day - 1)

die' :: String -> IO ()
die' s = do
  Ansi.hSetSGR stderr [Ansi.SetColor Ansi.Foreground Ansi.Vivid Ansi.Red ]
  die s

fetchInput :: Int -> Opts -> Day -> IO ()
fetchInput year opts day = do
    printf "Fetching input for day %v.\n" day
    cfg <- parseCfgFile (oCfg opts)
    case cfgToken cfg of
      Nothing -> die' "Can't fetch input: missing session token!"
      Just tok -> do
        response <- runReq defaultHttpConfig $
          req GET
              (https "adventofcode.com" /~ year /: "day" /~ fromIntegral @_ @Int day /: "input")
              NoReqBody
              bsResponse
              (sessTokenHeader tok)
        let outfile = printf "%s/day%.2d.txt" (oInputDataDir opts) day
        createDirectoryIfMissing True (oInputDataDir opts)
        ByteString.writeFile outfile (responseBody response)

sessTokenHeader :: Text -> Option scheme
sessTokenHeader tok = Req.header "Cookie" ("session=" <> Text.encodeUtf8 tok)

runTest :: RunTarget -> Solutions -> IO ()
runTest target solutions = case target of
  RunAll -> flip Vector.imapM_ solutions $ \i (S sln) ->
    runTestsOn (i+1) sln allParts
  RunSolution day -> case solutionForDay solutions day of
    Nothing -> die' $ printf "There is no solution for day %v!" day
    Just (S sln) -> runTestsOn (fromIntegral day) sln allParts
  RunSolutionPart day ps -> case solutionForDay solutions day of
    Nothing -> die' $ printf "There is no solution for day %v!" day
    Just (S sln) -> runTestsOn (fromIntegral day) sln ps

runTestsOn :: Int -> Solution i a b -> [Part] -> IO ()
runTestsOn day sln@Solution{tests,decodeInput} parts = do
  fgColor Ansi.Vivid Ansi.Blue
  printf "Running tests for day %v...\n" day
  for_ (zip [1..] tests) $ \(n :: Int, thisTest) -> do
    let (dyns, input, expected) = processTest thisTest
    fgColor Ansi.Vivid Ansi.Blue
    printf "  Test #%v\n" n
    case parse (decodeInput <* eof) "<test input>" . List.dropWhile isSpace . List.dropWhileEnd isSpace $ input of
      Left err -> do
        fgColor Ansi.Dull Ansi.Red
        printf "    Couldn't decode test input.\n"
        printf (errorBundlePretty err)
      Right dat -> for_ parts $ \part ->
        for_ (List.lookup part expected) $ \expectedResult ->
          let ?dyns = dyns in case runSolver sln part dat of
            Nothing -> do
              fgColor Ansi.Dull Ansi.Red
              printf "    %v: [X] No solution.\n" (displayPart part)
            Just result -> do
              if result == expectedResult
                then do
                  fgColor Ansi.Dull Ansi.Green
                  printf "    %v: [OK] Passed.\n" (displayPart part)
                else do
                  fgColor Ansi.Vivid Ansi.Red
                  printf "    %v: [X] Failed, expected: %v, got: %v\n" (displayPart part) expectedResult result

runSolve :: Opts -> RunTarget -> Bool -> Solutions -> IO ()
runSolve opts target upload solutions = do
  cfg <- parseCfgFile (oCfg opts)
  case target of
    RunAll -> flip Vector.imapM_ solutions $ \i (S sln) -> do
      runSolveOn (i+1) opts cfg upload sln allParts
    RunSolution day -> case solutionForDay solutions day of
      Nothing -> die' $ printf "There is no solution for day %v!" day
      Just (S sln) -> runSolveOn (fromIntegral day) opts cfg upload sln allParts
    RunSolutionPart day ps -> case solutionForDay solutions day of
      Nothing -> die' $ printf "There is no solution for day %v!" day
      Just (S sln) -> runSolveOn (fromIntegral day) opts cfg upload sln ps

runSolveOn :: Int -> Opts -> Cfg -> Bool -> Solution i a b -> [Part] -> IO ()
runSolveOn day opts cfg upload sln parts = do
  Ansi.setSGR []
  printf "Running solution for day %v...\n" day
  let infile = printf "%s/day%.2d.txt" (oInputDataDir opts) day
  input <- readFile infile
  case parse (decodeInput sln <* eof) infile . List.dropWhile isSpace . List.dropWhileEnd isSpace $ input of
    Left err -> do
      fgColor Ansi.Dull Ansi.Red
      printf "  Couldn't decode input! \n"
      printf (errorBundlePretty err)
    Right dat -> for_ parts $ \part -> do
      let ?dyns = emptyDynMap in case runSolver sln part dat of
        Nothing  -> do
          fgColor Ansi.Dull Ansi.Red
          printf "  %v: [X] No solution.\n" (displayPart part)
        Just result -> do
          fgColor Ansi.Dull Ansi.Green
          printf "  %v: [OK] The solution is:\n  %v\n" (displayPart part) result
          when upload $ do
            case cfgToken cfg of
              Nothing -> do
                fgColor Ansi.Dull Ansi.Red
                printf "Can't upload solution: missing session token!\n"
              Just _tok -> printf "Solution upload: not implemented\n"

maybeToRight :: e -> Maybe a -> Either e a
maybeToRight e = maybe (Left e) Right

instance PrintfArg (Finite n) where
  formatArg (Finite n) = formatArg n
  parseFormat (Finite n) = parseFormat n

readMFinite :: forall n. KnownNat n => ReadM (Finite n)
readMFinite = eitherReader $ maybeToRight errMsg . (packFinite <=< readMaybe)
  where errMsg = printf "Must be an integer between 0 and %d." (natVal @n Proxy)

readMParts :: ReadM [Part]
readMParts = eitherReader $ \s -> do
  parts <- for s \case
    'a' -> Right PartA
    'b' -> Right PartB
    _ -> Left "Not a valid part"
  pure if null parts then [PartA, PartB] else parts