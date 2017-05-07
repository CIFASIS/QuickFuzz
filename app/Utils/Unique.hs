module Utils.Unique where

import System.Environment
import System.FilePath
import System.Directory

import Data.Time

import Test.QuickFuzz.Gen.FormatInfo

import Args

-- |Return a unique filename
testName :: FormatInfo base actions -> IO String
testName fmt = do
    qf <- getProgName
    ts <- getTimestamp
    return $ qf <.> ts <.> ext fmt

-- |Return a unique timestamp
getTimestamp :: IO String
getTimestamp = 
    show . (`div` 1000000) . fromEnum . utctDayTime <$> getCurrentTime

-- |Return a function that generates uniquely parametrized filenames
-- Filenames looks like:
-- outdir/progName.timestamp.testNumber.genSeed.genSize.ext
nameMaker :: Show a => QFCommand -> FormatInfo base actions 
          -> IO (a -> a -> a -> String)
nameMaker cmd fmt = do
    qf <- getProgName
    ts <- getTimestamp
    return (\step seed size -> outDir cmd </>  qf 
        <.> ts <.> show step <.> show seed <.> show size <.> ext fmt)
