{-# LANGUAGE ViewPatterns #-}
module Utils.Generation where

import Prelude hiding (null)

import Data.ByteString.Lazy
import Data.Maybe

import Control.DeepSeq
import Control.Monad

import System.Random (randomIO)
import Test.QuickCheck.Random (mkQCGen)
import Test.QuickCheck.Gen

import Test.QuickFuzz.Gen.FormatInfo

import Args
import Debug
import Exception

-- Generate a reproductible value, retrying when the generated value fails to
-- enconde
strictGenerate :: (Show base) => QFCommand -> FormatInfo base actions -> Int 
               -> IO (Maybe actions, ByteString, Int)
strictGenerate cmd fmt n = do

    -- Generate a seed or use a given one if supplied
    seed <- if usesSeed cmd
                then return (fromJust (genSeed cmd))
                else abs <$> randomIO
    
    -- Generate a deterministic value using the previous seed 
    let (mbacts, val) = if hasActions fmt
        then let actionsGen = resize n (randomActions (getActionInfo fmt))
                 actsVal = unGen actionsGen (mkQCGen seed) seed
             in (Just actsVal, performActions (getActionInfo fmt) actsVal)
        else let baseGen = resize n (random fmt)
                 baseVal = unGen baseGen (mkQCGen seed) seed
             in (Nothing, baseVal)
    
    -- Try to evaluate the generated value, and retry if failed
    bsnf <- forceEvaluation (encode fmt val)
    case bsnf of
        Nothing -> do 
            debug "Encoding failure"
            when (usesSeed cmd) 
                 (error "given seed raised an encoding error. Aborting")
            strictGenerate cmd fmt n
        Just (null -> True) -> do 
            debug "Empty generation"
            when (usesSeed cmd) 
                 (error "given seed raised an encoding error. Aborting")
            strictGenerate cmd fmt n
        Just encoded -> do
            debug (show val)
            return (mbacts, encoded, seed)


-- |Generation size distributions
linear :: Int -> Int -> Int -> Int -> Int
linear minY maxY dx n = minY + floor (slope * fromIntegral ((n-1) `mod` dx)) 
    where slope = fromIntegral (maxY - minY + 1) / fromIntegral dx 

linearSize :: QFCommand -> Int -> Int 
linearSize cmd = linear (minSize cmd) (maxSize cmd) (genQty cmd) 

sawSize :: QFCommand -> Int -> Int
sawSize cmd = linear (minSize cmd) (maxSize cmd) (maxSize cmd - minSize cmd + 1)
