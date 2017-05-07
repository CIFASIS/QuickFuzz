module Run.Exec (runExec) where

import System.Directory
import System.FilePath

import Test.QuickFuzz.Gen.FormatInfo

import Args
import Debug

-- Run exec subcommand
runExec :: QFCommand -> FormatInfo base actions -> IO ()
runExec cmd fmt = putStrLn "Executing!" >> print cmd

getFiles :: QFCommand -> IO [FilePath]
getFiles cmd = undefined
    
