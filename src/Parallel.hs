module Parallel where

import Control.Concurrent.ParallelIO

import GHC.Conc -- Forcing parallelism

import Args

processPar :: (String -> MainArgs) -> (MainArgs -> IO ()) -> IO ()
processPar args process = do
    p <- getNumProcessors
    cap <- getNumCapabilities
    let i = p -1 -- Leave a proc free
    setNumCapabilities i  -- Forces to use p workers. i.e. -Nx not needed
    putStrLn $ "Procs : " ++ show p
    putStrLn $ "Capabilities: " ++ show cap
    let args' = map (\id -> args $ show id ) [1..i] 
    parallel_ (map process args')
