module Parallel where

import Control.Concurrent.ParallelIO

import GHC.Conc -- Forcing parallelism


processIN :: FilePath -> String -> String -> Int -> Int -> FilePath -> (FilePath -> String -> String -> Int -> Int -> FilePath -> IO ()) -> IO ()
processIN name cmd prop maxSuccess maxSize outdir process = do
    p <- getNumProcessors
    cap <- getNumCapabilities
    let i = p -1 -- Leave a proc free
    -- setNumCapabilities p  -- Forces to use p workers. i.e. -Nx not needed
    putStrLn $ "Procs : " ++ show p
    putStrLn $ "Capabilities: " ++ show cap
    let total = div maxSuccess i
    let names = map (\id -> show id ++ name) [1..p] 
    parallel_ (map (\x -> process x (cmd++(' ' : x)) prop total maxSize outdir) names)
