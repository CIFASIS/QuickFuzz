{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Wav where

import Args
import Test.QuickCheck
import Check

import Data.Binary( Binary(..), encode )

import Sound.Wav

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

import Data.DeriveTH
import Data.Word(Word8, Word16, Word32)
import Data.Int( Int16, Int8 )

import DeriveArbitrary
import ByteString
import Vector
import Images

import GHC.Types
import GHC.Word

import Data.List.Split


--instance Arbitrary AudioFormat where
--   arbitrary = oneof $ (map return [MicrosoftPCM, ])


$(deriveArbitraryRec ''WaveFile)

type MWaveFile  = WaveFile

mencode :: MWaveFile -> L.ByteString
mencode = encode

wavmain (MainArgs _ cmd filename prop maxSuccess maxSize outdir b) = let (prog, args) = (head spl, tail spl) in
    (case prop of
        "zzuf" -> quickCheckWith stdArgs { chatty = not b, maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ zzufprop filename prog args mencode outdir)
        "check" -> quickCheckWith stdArgs { chatty = not b, maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ checkprop filename prog args mencode outdir)
        "gen" -> quickCheckWith stdArgs { chatty = not b, maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ genprop filename prog args mencode outdir)
        "exec" -> quickCheckWith stdArgs { chatty = not b, maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ execprop filename prog args mencode outdir)
        _     -> error "Invalid action selected"
    ) where spl = splitOn " " cmd


main fargs False = wavmain $ fargs ""
main fargs True  = processPar fargs wavmain
