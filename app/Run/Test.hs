{-# LANGUAGE ViewPatterns #-}
module Run.Test (runTest)  where

import Prelude hiding (writeFile, length)

import Data.ByteString.Lazy hiding (putStrLn)
import Data.List hiding (length)
import Data.Maybe

import Control.Monad
import System.Directory

import Test.QuickFuzz.Gen.FormatInfo

import Args 
import Debug
import Exception
import Process
import Utils


-- |Return a lazy list of steps to execute
getSteps :: QFCommand -> [Int]
getSteps (maxTries -> Nothing) = [1..] 
getSteps (maxTries -> Just n) = [1..n] 


-- Run test subcommand
runTest :: (Show actions, Show base) => 
           QFCommand -> FormatInfo base actions -> IO ()
runTest cmd fmt = do
    debug (show cmd)
    when (hasActions fmt) 
         (putStrLn "Selected format supports actions based generation/shrinking!")

    createDirectoryIfMissing True (outDir cmd)
    
    mkName <- nameMaker cmd fmt
    (shcmd, testname) <- prepareCli cmd fmt

    let cleanup = when (usesFile cmd) $ removeFile testname

    -- Generation-execution-report loop
    forM_ (getSteps cmd) $ \n -> handleSigInt cleanup $ do
        
        let size = sawSize cmd n

        -- Generate a value and encode it in a bytestring.
        -- This fully evaluates the generated value, and retry
        -- the generation if the value cant be encoded.
        (mbacts, encoded, seed) <- strictGenerate cmd fmt size 

        -- Fuzz the generated value if required.
        fuzzed <- if usesFuzzer cmd 
            then fuzz (fromJust (fuzzer cmd)) encoded seed
            else return encoded
        
        -- Execute the command using either a file or stdin.
        exitcode <- if usesFile cmd
            then writeFile testname fuzzed >> execute (verbose cmd) shcmd 
            else executeFromStdin (verbose cmd) shcmd fuzzed 

        -- Report and move failed test cases.
        when (hasFailed exitcode) $ do
            let failname = mkName n seed size
            mapM_ putStrLn [ "Test case number " ++ show n ++ " has failed. "
                           , "Moving to " ++ failname ]
            if usesFile cmd
                then renameFile testname failname
                else writeFile failname fuzzed

        -- Shrink if necessary
        when (hasFailed exitcode && shrinking cmd) $ do 
          
            -- Execute a shrinking stategy acordingly to the -a/--actions flag
            (smallest, nshrinks, nfails) <- if hasActions fmt 
                then runShrinkActions cmd fmt shcmd testname 
                                      (fromJust mbacts) (diff encoded fuzzed)
                else runShrinkByteString cmd shcmd testname fuzzed 
            
            printShrinkingFinished
            
            -- Report the shrinking results
            let shrinkName = mkName n seed size ++ ".reduced"
            mapM_ putStrLn 
                [ "Reduced from " ++ show (length fuzzed)   ++ " bytes" 
                       ++ " to "  ++ show (length smallest) ++ " bytes"
                , "After executing " ++ show nshrinks ++ " shrinks with "
                       ++ show nfails ++ " failing shrinks. " 
                , "Saving to " ++ shrinkName ]
            writeFile shrinkName smallest
      
        when (not $ verbose cmd) (printTestStep n)
    
    -- Clean up the mess
    cleanup
    printFinished


