{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
module Args 
( module Args
, runApp
) where

import Data.Maybe
import Data.List
import Control.Monad
import Control.Applicative

import System.Directory
import System.Console.ArgParser
import System.Console.ArgParser.QuickParams

import Fuzzers

inputToken = "@@"

-- |Required to read Maybe parameters
instance RawRead a => RawRead (Maybe a) where
    rawParse s = do 
        (val, rem) <- rawParse s 
        return (Just val, rem)

-- |Required to read Fuzzer parameter
instance RawRead Fuzzer where
    rawParse (stripPrefix "zzuf"    -> Just rem) = Just (Zzuf, rem)
    rawParse (stripPrefix "radamsa" -> Just rem) = Just (Radamsa, rem)
    rawParse _ = Nothing

-- |The data type representing the actions
-- that QuickFuzz can perform
data QFCommand 
    = Test     -- ^Generate, maybe fuzz, execute and maybe shrink   
        { format :: String
        , cli :: String
        , verbose :: Bool
        , timeout :: Maybe Int
        , shrinking :: Bool
        , fuzzer :: Maybe Fuzzer
        , singleFail :: Bool
        , genSeed :: Maybe Int
        , maxTries :: Maybe Int
        , minSize :: Int
        , maxSize :: Int
        , outDir :: String
        , outFile :: Maybe String }
    | Gen       -- ^Generate some test cases
        { format :: String
        , genSeed :: Maybe Int
        , minSize :: Int
        , maxSize :: Int
        , genQty :: Int
        , outDir :: String }
    | Serve     -- ^Serve some data using a socket
        { format :: String
        , port    :: Int
        , minSize :: Int
        , maxSize :: Int
        , genQty  :: Int
        , outDir :: String }
    | Exec     -- ^Execute command using a given data set, maybe shrikning
        { format :: String
        , cli :: String
        , verbose :: Bool
        , timeout :: Maybe Int
        , shrinking :: Bool
        , fuzzer :: Maybe Fuzzer
        , singleFail :: Bool
        , inDir :: String
        , outDir :: String
        , outFile :: Maybe String }
    | Shrink   -- ^Shrink a given set of test cases
        { format :: String
        , cli :: String
        , verbose :: Bool
        , timeout :: Maybe Int 
        , inDir :: String
        , outDir :: String
        , outFile :: Maybe String }
    | List     -- ^List supported data types
    deriving Show


-- | Main parsing function
parseCommand :: IO (CmdLnInterface QFCommand)
parseCommand = setVersion "1.0.0"
           <$> setDescr "An experimental grammar fuzzer in Haskell using QuickCheck."
           <$> setEpilog "More info: QuickFuzz.org"             
           <$> parseSubcommand

-- | Compose main parser using subcommand parsers
parseSubcommand :: IO (CmdLnInterface QFCommand)
parseSubcommand = mkSubParser 
    [ ("test", mkDefaultApp testParser "test") 
    , ("gen", mkDefaultApp genParser "gen") 
    , ("serve", mkDefaultApp serveParser "serve") 
    , ("exec", mkDefaultApp execParser "exec") 
    , ("reduce", mkDefaultApp shrinkParser "reduce") 
    , ("list", mkDefaultApp listParser "list") ]

-- | Test subcommand parser
testParser :: ParserSpec QFCommand
testParser = Test
    `parsedBy` reqPos           "format"   `Descr` "File format to generate (run list option to see available formats)"
    `andBy`    reqPos           "command"  `Descr` "Command line to execute"
    `andBy`    boolFlag         "verbose"  `Descr` "Print execution output"
    `andBy`    optFlag Nothing  "timeout"  `Descr` "Set a timeout to prevent long executions"
    `andBy`    boolFlag         "reduce"   `Descr` "Reduce crash-inducing test cases"
    `andBy`    optFlag Nothing  "fuzzer"   `Descr` "Set a fuzzer to mutate generated test cases before execution"
    `andBy`    boolFlag         "keep"     `Descr` "Keep testing after finding a crashing test case"
    `andBy`    optFlag Nothing  "seed"     `Descr` "Generate using a given integer seed"
    `andBy`    optFlag Nothing  "quantity" `Descr` "Maximum number of tries"
    `andBy`    optFlag 1        "lower"    `Descr` "Minimum generation size"
    `andBy`    optFlag 50       "upper"    `Descr` "Maximum generation size"
    `andBy`    optFlag "outdir" "outdir"   `Descr` "Output directory"
    `andBy`    optFlag Nothing  "name"     `Descr` "Output filename"

-- | Gen subcommand parser
genParser :: ParserSpec QFCommand 
genParser = Gen 
    `parsedBy` reqPos           "format"   `Descr` "File format to generate (run list option to see available formats)"
    `andBy`    optFlag Nothing  "seed"     `Descr` "Generate using a given integer seed"
    `andBy`    optFlag 1        "lower"    `Descr` "Minimum generation size"
    `andBy`    optFlag 50       "upper"    `Descr` "Maximum generation size"
    `andBy`    optFlag 1000     "quantity" `Descr` "Number of generated files"
    `andBy`    optFlag "outdir" "outdir"   `Descr` "Output directory"


-- | Serve subcommand parser
serveParser :: ParserSpec QFCommand 
serveParser = Serve 
    `parsedBy` reqPos           "format"   `Descr` "File format to generate (run list option to see available formats)"
    `andBy`    reqPos           "port"     `Descr` "Port number to bind"
    `andBy`    optFlag 1        "lower"    `Descr` "Minimum generation size"
    `andBy`    optFlag 50       "upper"    `Descr` "Maximum generation size"
    `andBy`    optFlag 1000     "quantity" `Descr` "Number of generated files"
    `andBy`    optFlag "outdir" "outdir"   `Descr` "Output directory"


-- | Exec subcommand parser
execParser :: ParserSpec QFCommand
execParser = Exec
    `parsedBy` reqPos           "format"   `Descr` "File format to execute (run list option to see available formats)"
    `andBy`    reqPos           "command"  `Descr` "Command line to execute"
    `andBy`    boolFlag         "verbose"  `Descr` "Print execution output"
    `andBy`    optFlag Nothing  "timeout"  `Descr` "Set a timeout to prevent long executions"
    `andBy`    boolFlag         "reduce"   `Descr` "Reduce crash-inducing test cases"
    `andBy`    optFlag Nothing  "fuzzer"   `Descr` "Set a fuzzer to mutate generated test cases before execution"
    `andBy`    boolFlag         "keep"     `Descr` "Keep testing after finding a crashing test case"
    `andBy`    optFlag "indir"  "inputs"   `Descr` "Path to inputs test cases"
    `andBy`    optFlag "outdir" "outdir"   `Descr` "Output directory"
    `andBy`    optFlag Nothing  "name"     `Descr` "Output filename"

-- | Shrink subcommand parser
shrinkParser :: ParserSpec QFCommand
shrinkParser = Shrink
    `parsedBy` reqPos           "format"   `Descr` "File format to generate (run list option to see available formats)"
    `andBy`    reqPos           "command"  `Descr` "Command line to execute"
    `andBy`    boolFlag         "verbose"  `Descr` "Print execution output"
    `andBy`    optFlag Nothing  "timeout"  `Descr` "Set a timeout to prevent long executions"
    `andBy`    optFlag "indir"  "inputs"   `Descr` "Path to inputs test cases"
    `andBy`    optFlag "outdir" "outdir"   `Descr` "Output directory"
    `andBy`    optFlag Nothing  "name"     `Descr` "Output filename"

-- | List subcommand parser
listParser = pure List 

-- |Command attributes shortcuts
usesFile :: QFCommand -> Bool
usesFile = isInfixOf inputToken . cli

usesTimeout :: QFCommand -> Bool
usesTimeout = isJust . timeout

usesFuzzer :: QFCommand -> Bool
usesFuzzer = isJust . fuzzer

usesTriesLimit :: QFCommand -> Bool
usesTriesLimit = isJust . maxTries

usesOutFile :: QFCommand -> Bool
usesOutFile = isJust . outFile

usesSeed :: QFCommand -> Bool
usesSeed = isJust . genSeed

-- |Args sanitizing, e.g: check lower/upper bounds,
-- check executable existense, etc.
sanitize :: QFCommand -> IO QFCommand
sanitize cmd@(Test {}) = checkBounds cmd >>= checkSeedTries >>= checkMaxTries 
sanitize cmd@(Gen {}) = checkGenQty cmd >>= checkBounds >>= checkSeedQty  
sanitize cmd@(Exec {}) = checkInDir cmd  
sanitize cmd@(Shrink {}) = checkInDir cmd 
sanitize cmd@(Serve {}) = checkGenQty cmd >>= checkBounds 
sanitize List = return List 

checkGenQty :: QFCommand -> IO QFCommand
checkGenQty cmd = do
    when (genQty cmd < 1) (error "Generation quantity must be positive")
    return cmd

checkMaxTries :: QFCommand -> IO QFCommand
checkMaxTries cmd@(maxTries -> Nothing) = return cmd
checkMaxTries cmd@(maxTries -> Just tries) = do
    when (tries < 1) (error "Maximum number of tries must be positive")
    return cmd 

checkSeedTries :: QFCommand -> IO QFCommand
checkSeedTries cmd@(genSeed -> Nothing) = return cmd
checkSeedTries cmd@(genSeed -> Just seed) = do
    when (seed < 0) (error "Seed must be non-negative")
    return $ cmd {maxTries = Just 1}
        
checkSeedQty :: QFCommand -> IO QFCommand
checkSeedQty cmd@(genSeed -> Nothing) = return cmd
checkSeedQty cmd@(genSeed -> Just seed) = do
    when (seed < 0) (error "Seed must be non-negative")
    return $ cmd {genQty = 1}

checkInDir :: QFCommand -> IO QFCommand
checkInDir cmd = do
    inDirExist <- doesDirectoryExist (inDir cmd)
    when (not inDirExist) (error "Input directory does not exist")
    return cmd

checkBounds :: QFCommand -> IO QFCommand
checkBounds cmd = do
    when (minSize cmd <= 0) (error "Minimum generation size must be positive")
    when (maxSize cmd <= 0) (error "Maximum generation size must be positive")
    let [sMin, sMax] = sort [minSize cmd, maxSize cmd]
    return $ cmd {minSize = sMin, maxSize = sMax}

checkExe :: QFCommand -> IO QFCommand
checkExe cmd = do
    let name = head (words (cli cmd))    
    exe <- findExecutable name 
    when (isNothing exe) (error $ "Executable " ++ name ++ " not found. ")
    return cmd 
