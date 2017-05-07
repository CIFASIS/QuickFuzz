module Test.QuickFuzz.Global where

import System.IO.Unsafe (unsafePerformIO)

import Control.Exception
import Data.Global
import Data.IORef
import Data.Maybe

type FreqVar = IORef [Int]
type FreqState = IORef [(String, FreqVar)]

freqs :: FreqState
freqs = declareIORef "freqs" []

declareFreqVar var = (var, declareIORef var ([] :: [Int]))

readFreqFile :: IO String
readFreqFile = do
   strOrExc <- try $ readFile "freqs.txt"
   case (strOrExc :: Either IOError String) of
     Left _         -> return "[]"
     Right contents -> return contents

initFreqs :: IO ()
initFreqs = do 
              contents <- readFreqFile
              let freqMap = (read contents) :: [(String, [Int])]
 
              updateVar freqs (map (\(var, xs) -> declareFreqVar var) freqMap)

              freqs <- readIORef freqs
              mapM_ (\(var, xs) -> setFreqs var xs freqs ) freqMap


getFreqs :: String -> [Int]
getFreqs var = unsafePerformIO $ do
                                   freqs <- readIORef freqs
                                   --print var
                                   case (lookup var freqs) of
                                     Just x -> readIORef x
                                     Nothing -> return (repeat 1)

setFreqs :: String -> [Int] -> [(String, FreqVar)] -> IO ()
setFreqs var xs freqs = updateVar (fromJust (lookup var freqs)) xs

updateVar :: (IORef a) -> a -> IO ()
updateVar v xs = atomicModifyIORef v f
  where f _ = (xs, ())

