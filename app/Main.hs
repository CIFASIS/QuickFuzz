{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Exception
import System.Environment
import Data.List

import Args
import Formats
import Utils

import DeriveDispatcher

import Run.Test
import Run.Gen
import Run.Exec
import Run.Shrink
import Run.List
import Run.Serve

import Test.QuickFuzz.Global

-- |Print unsupported file format error message
unsupported :: String -> IO ()
unsupported fmt = do
    qf <- getProgName
    error $ "file format '" ++ fmt ++ "' is not supported.\n" ++
            "See '" ++ qf ++ " list' to print the available formats."  

-- |Template Haskell dispatcher derivations
devDispatcher 'Test   'runTest
devDispatcher 'Gen    'runGen
devDispatcher 'Exec   'runExec
devDispatcher 'Shrink 'runShrink
devDispatcher 'Serve  'runServe

-- |Subcommands dispatcher
quickfuzz :: QFCommand -> IO ()
quickfuzz cmd@(Test {}) = dispatchTest cmd
quickfuzz cmd@(Gen {}) = dispatchGen cmd
quickfuzz cmd@(Exec {}) = dispatchExec cmd
quickfuzz cmd@(Shrink {}) = dispatchShrink cmd
quickfuzz cmd@(Serve {}) = dispatchServe cmd
quickfuzz List = runList



-- |Pretty print error messages
printException :: SomeException -> IO ()
printException e = do
    restoreTerm
    qf <- getProgName
    putStrLn $ qf ++ ": " ++ show e

main :: IO ()
main = handle printException $ do
    initFreqs
    command <- parseCommand
    cleanTerm
    runApp command (\cmd -> sanitize cmd >>= quickfuzz)
    restoreTerm
