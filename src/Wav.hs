{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Wav where

import Test.QuickCheck
import Data.Binary( Binary(..), encode )

import Sound.Wav

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

import Data.DeriveTH

import DeriveArbitrary
import ByteString
import Vector

instance Arbitrary AudioFormat where
   arbitrary = oneof $ (map return [MicrosoftPCM])

$(deriveArbitraryRec ''WaveFile)

type MWaveFile  = WaveFile

mencode :: MWaveFile -> L.ByteString
mencode = encode
