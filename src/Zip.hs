{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Zip where

import Test.QuickCheck
import Check
import DeriveArbitrary

import Data.Binary( Binary(..), encode )

import Codec.Archive.Zip

import qualified Data.ByteString.Lazy as L
import Data.Word(Word8, Word16, Word32)

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS

import Vector
import ByteString

$(deriveArbitraryRec ''Archive)

mencode :: Archive -> L.ByteString
mencode = encode 

main = quickCheckWith stdArgs { maxSuccess = 120000, maxSize = 50 } (checkprop "buggy_qc.zip" "/bin/tar" ["-tZf", "buggy_qc.zip"] mencode)
