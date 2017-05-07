module Run.Gen (runGen) where

import Prelude hiding (writeFile)

import Data.ByteString.Lazy

import Control.Monad

import System.FilePath
import System.Directory

import Test.QuickCheck (generate, resize)
import Test.QuickFuzz.Gen.FormatInfo

import Args
import Debug
import Exception
import Utils

-- |Run gen subcommand
runGen :: (Show actions, Show base) =>
          QFCommand -> FormatInfo base actions -> IO ()
runGen cmd fmt = do 
    debug (show cmd)
    mkName <- nameMaker cmd fmt
    
    createDirectoryIfMissing True (outDir cmd)
    let steps | usesSeed cmd = [0]
              | otherwise    = [0..genQty cmd]

    forM_ steps $ \n -> handleSigInt (return ()) $ do 
       
        let size = linearSize cmd n

        (mbacts, encoded, seed) <- strictGenerate cmd fmt size
        writeFile (mkName n seed size) encoded
        
        printGenStep n

    printFinished
