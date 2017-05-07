module Utils.Console where

import Data.Maybe
import Data.List
import Data.List.Split

import System.FilePath
import System.Environment

import Control.Concurrent
import Control.Exception
import System.IO.Error
import System.Posix.Signals
import System.Console.ANSI

import Test.QuickFuzz.Gen.FormatInfo
import Args

import Utils.Unique

-- | Generates the shell command needeed to execute the target program,
-- inserting a user provided constant output filename, or generating a unique
-- one otherwise. 
prepareCli :: QFCommand -> FormatInfo base actions -> IO ((FilePath, [String]), String)
prepareCli cmd fmt 
    | usesOutFile cmd = do
        let filename = outDir cmd </> fromJust (outFile cmd)
        return (mkRawCli (cli cmd) filename fmt, filename)  
    | otherwise = do
        filename <- testName fmt
        return (mkRawCli (cli cmd) filename fmt, filename) 

mkRawCli cli fname fmt = (head tokens, tail tokens)
    where tokens = wordsBy (==' ') $ replace fname $ cli
          replace fname = intercalate fname . splitOn inputToken


-- |Add some behavior when a sigterm is invoked by the user
handleSigInt :: IO () ->  IO () -> IO ()
handleSigInt cleaner action = do
    tid <- myThreadId
    let interruptedError = userError "Operation interrupted by the user"
        handler = cleaner >> throwTo tid (toException interruptedError)  
    installHandler keyboardSignal (Catch handler) Nothing
    action

-- |Console printers and helpers
printTestStep n = do
    clearLine
    putStrLn $ show n ++  " random files tested."
    cursorUpLine 1

printGenStep n = do 
    clearLine
    putStrLn $ show n ++  " files generated."
    cursorUpLine 1

printShrinkStep size = do
--    clearLine
    putStrLn $ "Testing shrink of size " ++ show size
--    cursorUpLine 1

printShrinkingFinished = do
    putStrLn "Shrinking finished"

cleanTerm = clearLine >> hideCursor
restoreTerm = clearLine >> showCursor 
printFinished = cursorDownLine 1 >> putStrLn "Finished!" >> showCursor
