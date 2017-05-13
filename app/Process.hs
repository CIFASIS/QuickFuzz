{-# LANGUAGE ViewPatterns #-}

module Process where

import Prelude hiding (putStrLn)

import Control.Monad
import Data.ByteString.Lazy.Char8

import System.Process.ByteString.Lazy (readProcessWithExitCode)
import System.Process (rawSystem)
import System.Exit
import System.FilePath
import System.IO hiding (putStrLn)
import System.IO.Silently
import System.Random

import Fuzzers
import Exception

-- |Check if a shell exit code represents a failed execution
hasFailed :: ExitCode -> Bool
hasFailed (ExitFailure n) = n < 0 || (n > 128 && n < 143)
hasFailed ExitSuccess = False

-- |A shell command consists in a filepath to the executable
-- and a list of arguments
type ShellCommand = (FilePath, [String])


-- |Execute a shell command, as faster as possible.
-- If a verbose flag is set, the process stdout is not silenced.
execute :: Bool -> ShellCommand -> IO ExitCode
execute verbose (exe, args) =  addVerbosity $ rawSystem exe args
    where addVerbosity | verbose   = id 
                       | otherwise = hSilence [stdout, stderr]


-- |Execute a shell command passing it a bytestring as stdin
-- and returning its exit code. 
executeFromStdin :: Bool -> ShellCommand -> ByteString -> IO ExitCode
executeFromStdin verbose (exe, args) bs = do
    (exitCode, stdout, stderr) <- readProcessWithExitCode exe args bs 
    when verbose $ putStrLn stdout
    return exitCode

-- |Fuzz a bytestring into another using a given fuzzer.
-- If something fails, return the input unaltered
fuzz :: Fuzzer -> ByteString -> Int -> IO ByteString
fuzz fuzzer bs seed = do
    let (exe, args) = getFuzzerCommand fuzzer seed
    (_,fuzzed,_) <- readProcessWithExitCode exe args bs 
    return fuzzed 
        
