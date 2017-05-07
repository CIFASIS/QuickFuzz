module Run.Shrink (runShrink) where

import Control.Monad

import Test.QuickFuzz.Gen.FormatInfo

import Args
import Debug

-- Run shrink subcommand
runShrink :: QFCommand -> FormatInfo base actions ->  IO ()
runShrink cmd fmt = do
    when (hasActions fmt) 
         (putStrLn "Selected format supports actions base generation/shrinking!")
