module Utils.Shrink where

import Prelude hiding (init, inits, tail, tails, writeFile, length)

import Data.ByteString.Lazy hiding (putStrLn, zip)
import Control.Concurrent.MVar
import Control.Monad

import Test.QuickCheck hiding (verbose) 

import Test.QuickCheck.Monadic
import Test.QuickFuzz.Gen.FormatInfo

import Args hiding (shrinking)
import Process

import Utils.Patch
import Utils.Console

-- |ByteString generic shrinking
shrinkBS :: ByteString -> [ByteString]
shrinkBS bs = 
    [ append h t | (h, t) <- zip (inits (init bs)) (tails (tail bs)) ] 


runShrinkActions :: (Show actions) => QFCommand -> FormatInfo base actions
                 -> ShellCommand -> FilePath -> actions -> [Patch]
                 -> IO (ByteString, Int, Int)
runShrinkActions cmd fmt shcmd testname acts delta = do
    putStrLn "Shrinking over actions has begun..."
    putStrLn "Action to reduce:"
    print acts
                
    let shrinkActs = shrinkActions (getActionInfo fmt)
        encodeActs = encode fmt . performActions (getActionInfo fmt)
    
        runShrink acts = runShrink' acts (shrinkActs acts) 0 0
                    
        runShrink' last [] nshrinks nfails = return (last, nshrinks, nfails)
        runShrink' last (x:xs) nshrinks nfails = do
        
            let enc = encodeActs x
                encoded = if usesFuzzer cmd 
                            then patch delta enc
                            else enc
            print x
            printShrinkStep (length encoded)
                        
            exitcode <- if usesFile cmd
                then writeFile testname encoded >> execute (verbose cmd) shcmd 
                else executeFromStdin (verbose cmd) shcmd encoded

            if hasFailed exitcode
                then runShrink' x (shrinkActs x) (nshrinks+1) (nfails+1)
                else runShrink' last xs (nshrinks+1) nfails

    (smallestActs, nshrinks, nfails) <- runShrink acts

    let encoded = if usesFuzzer cmd 
        then patch delta (encodeActs smallestActs)
        else encodeActs smallestActs
            
    return (encoded, nshrinks, nfails)


runShrinkByteString :: QFCommand -> ShellCommand -> FilePath 
                    -> ByteString -> IO (ByteString, Int, Int)
runShrinkByteString cmd shcmd testname fuzzed  = do 
    putStrLn "Shrinking over bytes has begun..."
    
    let qcArgs = stdArgs {chatty = False}
        shrinkingProp = shrinking shrinkBS fuzzed
    
    -- Create an MVar to take the smallest shrink from inside of the QuickCheck
    -- loop, and also count the number of shinks and intermediate fails.
    shrinkStatus <- newMVar (fuzzed, 0, 0)
    
    -- Run QuickCheck using a shrinking property
    quickCheckWith qcArgs $ shrinkingProp $ \shrink ->
        monadicIO $ do
            run $ printShrinkStep (length shrink)
            
            -- Execute the shrink
            exitcode <- run $ if usesFile cmd
                then writeFile testname shrink >> execute (verbose cmd) shcmd 
                else executeFromStdin (verbose cmd) shcmd shrink 
                    
            -- When a shrink fails, update the MVar with the value.
            if hasFailed exitcode 
                then run $ modifyMVar_ shrinkStatus 
                        (\(_, shn, shf) -> return (shrink, shn + 1, shf + 1))
                else run $ modifyMVar_ shrinkStatus 
                        (\(sh, shn, shf) -> return (sh, shn + 1, shf))
            
            -- Tell QuickCheck if the case has failed
            assert $ not $ hasFailed exitcode
                
    (smallest, nshrinks, nfails) <- takeMVar shrinkStatus
    return (smallest, nshrinks, nfails)
