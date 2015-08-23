{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Zip where

import Test.QuickCheck
import Check
import DeriveArbitrary

import Data.Binary( Binary(..), encode )

import Codec.Archive.Zip

import qualified Data.ByteString.Lazy as L
import Data.DeriveTH
import Data.Word(Word8, Word16, Word32)
import Data.Int( Int16, Int8 )

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS

import System.Process
import System.Exit
import System.Random

import Vector
import ByteString

instance Arbitrary (V.Vector (VU.Vector Word8)) where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Word8)
     return $ V.replicate 32 (VU.fromList l)

{-
instance Arbitrary L.ByteString where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Word8)
     return $ L.pack l
-}

$(deriveArbitraryRec ''Archive)

mencode :: Archive -> L.ByteString
mencode = encode 

main = quickCheckWith stdArgs { maxSuccess = 1200, maxSize = 10 } (fuzzprop "buggy_qc.zip" "/usr/bin/unzip" ["-l", "buggy_qc.zip"] mencode)
