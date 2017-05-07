module Run.List (runList) where

import Data.List
import Data.Ord

import Args
import Debug
import Formats

-- Run list subcommand
runList :: IO ()
runList = do
    putStrLn "Supported formats:"
    mapM_ (putStrLn . fst) (sortBy (comparing fst) formats) 


