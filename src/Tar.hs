{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Tar where

import Test.QuickCheck
import Check
import DeriveArbitrary


import Control.Monad.Zip
import Control.Exception
import Data.Binary( Binary(..), encode )

import Codec.Archive.Tar.Entry
import Codec.Archive.Tar.Types
import Codec.Archive.Tar.Write

import System.Posix.Types

import qualified Data.ByteString.Lazy as L
import Data.DeriveTH
import Data.Word(Word8, Word16, Word32)
import Data.Int( Int16, Int8 )

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS

import System.Process
import System.Exit


instance Arbitrary (V.Vector Word32) where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Word32)
     return $ V.fromList l

instance Arbitrary (V.Vector Word16) where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Word16)
     return $ V.fromList l

instance Arbitrary (VU.Vector Int) where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Int)
     return $ VU.fromList l

instance Arbitrary (VU.Vector Int8) where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Int8)
     return $ VU.fromList l

instance Arbitrary (VU.Vector Word8) where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Word8)
     return $ VU.fromList l

instance Arbitrary (VS.Vector Word16) where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Word16)
     return $ VS.fromList l

instance Arbitrary (V.Vector (VU.Vector Word8)) where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Word8)
     s <- (arbitrary :: Gen Int)
     return $ V.replicate s (VU.fromList l)

-- $(deriveArbitraryRec ''Entry) (not working)

derive makeArbitrary ''Entry
derive makeShow ''Entry

derive makeArbitrary ''EntryContent
derive makeArbitrary ''TarPath
derive makeShow ''EntryContent
derive makeShow ''TarPath
derive makeArbitrary ''LinkTarget
derive makeShow ''LinkTarget
derive makeArbitrary ''Format
derive makeArbitrary ''Ownership
derive makeShow ''Format
derive makeShow ''Ownership

mencode ::  [Entry] -> L.ByteString
mencode = write

instance Arbitrary L.ByteString where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Word8)
     return $ L.pack []

instance Arbitrary Permissions where
   arbitrary = do
     w32 <- arbitrary :: Gen Word32
     return $ CMode w32

main = quickCheckWith stdArgs { maxSuccess = 12000000, maxSize = 500 } (noShrinking $ absprop "buggy_qc.tar" "/bin/tar" ["-tvf", "buggy_qc.tar.fuzzed"] mencode)
