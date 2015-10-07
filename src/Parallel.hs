module Parallel where

import Control.Concurrent.ParallelIO

-- No muy inteligente, pero empecemos a jugar un poco con la herramienta

processIN :: Int -> FilePath -> String -> String -> Int -> Int -> FilePath -> (FilePath -> String -> String -> Int -> Int -> FilePath -> IO ()) -> IO ()
processIN i name cmd prop maxSuccess maxSize outdir process = do
    let names = map (\id -> show id ++ name) [1..i] 
    parallel_ (map (\x -> process x (cmd++(' ' : x)) prop maxSuccess maxSize outdir) names)
