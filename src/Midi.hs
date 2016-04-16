{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Midi where

import Test.QuickCheck
import DeriveArbitrary
import Strings

import Codec.Midi
import Codec.ByteString.Builder

import qualified Data.ByteString.Lazy as L

type MMidi = Midi


instance Arbitrary String where
   arbitrary = mgenName

$(devArbitrary ''MMidi)

mencode :: MMidi -> L.ByteString
mencode m = toLazyByteString $ buildMidi m
