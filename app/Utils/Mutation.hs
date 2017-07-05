{-# LANGUAGE ViewPatterns #-}
module Utils.Mutation where

import Prelude hiding (null)
import Data.Maybe
import Data.ByteString.Lazy

import Control.DeepSeq
import Control.Monad

import System.Random (randomIO)
import Test.QuickCheck.Random (mkQCGen)
import Test.QuickCheck.Gen

import Test.QuickFuzz.Gen.FormatInfo

import Args
import Debug
import Exception

import System.Directory hiding (listDirectory, withCurrentDirectory)
import Control.Exception ( bracket )

import Test.QuickCheck.Random (mkQCGen)
import Test.QuickCheck.Gen
import System.Random (randomIO)

sampleGenerator gen n seed = 
  let baseGen = resize n gen
      baseVal = unGen baseGen (mkQCGen seed) seed
  in baseVal

-- Mutatue a reproductible value, retrying when the generated value fails to
-- enconde
strictMutate :: (Show base, NFData base) => QFCommand -> FormatInfo base actions 
               -> [base] -> Int -> IO (ByteString, Int)
strictMutate cmd fmt values n = do
        
        seed <- if usesSeed cmd
                then return (fromJust (genSeed cmd))
                else abs <$> randomIO

        idx <- return $ (seed `mod` (Prelude.length values))
        value <- return (values !! idx)
        --print value
 
        -- Mutate some value.
        mutated <- return $ sampleGenerator ((mutate fmt) value) n seed

        if ( show value == show mutated ) 
        then strictMutate cmd fmt values n
        else ( do
            bsnf <- forceEvaluation (encode fmt mutated)
            case bsnf of
              Nothing -> do 
                debug "Encoding failure"
                when (usesSeed cmd) 
                    (error "given seed raised an encoding error. Aborting")
                strictMutate cmd fmt values n
              Just (null -> True) -> do 
                debug "Empty generation"
                when (usesSeed cmd) 
                    (error "given seed raised an encoding error. Aborting")
                strictMutate cmd fmt values n
              Just encoded -> do
                --debug (show val)
                return (encoded, seed)
        
             )

