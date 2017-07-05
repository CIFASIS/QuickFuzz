{-# LANGUAGE ViewPatterns #-}
module Run.MutTest (runMutTest)  where

import Prelude hiding (writeFile)

import Data.ByteString.Lazy hiding (putStrLn)
import Data.List hiding (length)
import Data.Maybe
import GHC.Int

import Control.Monad
import Control.DeepSeq
import System.Directory

import Test.QuickFuzz.Gen.FormatInfo

import Args 
import Debug
import Exception
import Process
import Utils

import Utils.Decoding
import Utils.Mutation

import Test.QuickCheck.Random (mkQCGen)
import Test.QuickCheck.Gen
import System.Random (randomIO)

-- |Return a lazy list of steps to execute
getSteps :: QFCommand -> [Int]
getSteps (maxTries -> Nothing) = [1..] 
getSteps (maxTries -> Just n) = [1..n] 

-- Run test subcommand
runMutTest :: (Show actions, Show base, NFData base) => 
           QFCommand -> FormatInfo base actions -> IO ()
runMutTest cmd fmt = do
    debug (show cmd)
    when (hasActions fmt) 
         (putStrLn "Selected format supports actions based generation/shrinking!")

    createDirectoryIfMissing True (outDir cmd)
    values <- strictDecode cmd fmt
    
    mkName <- nameMaker cmd fmt
    (shcmd, testname) <- prepareCli cmd fmt

    let cleanup = when (usesFile cmd) $ removeFile testname

    -- Generation-execution-report loop
    forM_ (getSteps cmd) $ \n -> handleSigInt cleanup $ do
        
        let size = sawSize cmd n

        -- Generate a value and encode it in a bytestring.
        -- This fully evaluates the generated value, and retry
        -- the generation if the value cant be encoded.

        -- Mutate some value.
        (mutated, seed) <- strictMutate cmd fmt values size
        
        -- Execute the command using either a file or stdin.
        exitcode <- if usesFile cmd
            then writeFile testname mutated >> execute (verbose cmd) shcmd 
            else executeFromStdin (verbose cmd) shcmd mutated 

        -- Report and move failed test cases.
        when (hasFailed exitcode) $ do
            let failname = mkName n seed size
            mapM_ putStrLn [ "Test case number " ++ show n ++ " has failed. "
                           , "Moving to " ++ failname ]
            if usesFile cmd
                then renameFile testname failname
                else writeFile failname mutated

        -- Shrink if necessary
        {-
        when (hasFailed exitcode && shrinking cmd) $ do 
          
            -- Execute a shrinking stategy acordingly to the -a/--actions flag
            (smallest, nshrinks, nfails) <- if hasActions fmt 
                then runShrinkActions cmd fmt shcmd testname 
                                      (fromJust mbacts) (diff encoded mutated)
                else runShrinkByteString cmd shcmd testname mutated 
            
            printShrinkingFinished
            
            -- Report the shrinking results
            let shrinkName = mkName n seed size ++ ".reduced"
            mapM_ putStrLn 
                [ "Reduced from " ++ show (length mutated)   ++ " bytes" 
                       ++ " to "  ++ show (length smallest) ++ " bytes"
                , "After executing " ++ show nshrinks ++ " shrinks with "
                       ++ show nfails ++ " failing shrinks. " 
                , "Saving to " ++ shrinkName ]
            writeFile shrinkName smallest
        -}
        when (not $ verbose cmd) (printTestStep n)
    
    -- Clean up the mess
    cleanup
    printFinished


