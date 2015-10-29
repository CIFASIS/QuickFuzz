module Parallel where

import Control.Concurrent.ParallelIO

import GHC.Conc -- Forcing parallelism

import Args

import Test.QuickCheck.Test

import Control.Monad.Par.IO
import Control.Monad.Par.Class
import Control.Monad.IO.Class
import Control.DeepSeq
import Control.Monad

instance NFData Result where
    rnf x = seq (isSuccess x) () -- TODO improve?

processPar :: (String -> MainArgs) -> (MainArgs -> IO a) -> IO ()
processPar args process = do
    p <- getNumProcessors
    cap <- getNumCapabilities
    setNumCapabilities p  -- Forces to use p workers. i.e. -Nx not needed
    parallel_ (map (process . args . show)[1..(p*4)])

parallelism :: (String -> MainArgs) -> (MainArgs -> IO Result) -> IO ()
parallelism args process = do
    p <- getNumProcessors
    cap <- getNumCapabilities
    setNumCapabilities p  -- Forces to use p workers. i.e. -Nx not needed
    runParIO $ do
            xs <- foldM (\xs x -> do
                        let args' = args $ show x
                        v <- new
                        fork ((liftIO $ process args') >>= put v)
                        return (v:xs)) [] [1..(p*4)]
            mapM_ get xs
