{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module QuickZip where

import Test.QuickCheck
--import Test.QuickCheck.Instances

import Control.Monad.Zip
import Control.Exception
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

import Test.QuickCheck.Monadic (assert, monadicIO, run)

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
     return $ V.replicate 32 (VU.fromList l)

instance Arbitrary L.ByteString where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Word8)
     return $ L.pack l



derive makeArbitrary ''Archive
derive makeArbitrary ''Entry
derive makeArbitrary ''CompressionMethod

handler :: SomeException -> IO ()
handler _ = return ()

prop :: Archive -> Property
prop x = monadicIO $ do
         run $ Control.Exception.catch (L.writeFile "buggy_qc.zip" (encode x)) handler
         r <- run (randomIO :: IO Int)
         ret <- run $ rawSystem "/usr/bin/zzuf" ["-s", (show (r `mod` 100024))++":"++(show (r `mod` 100024 + 5000)), "-q", "-M", "-1", "-c", "-S", "-T", "60", "-j", "5", "../unzip610c19/unzip", "-l", "buggy_qc.zip"]
         case ret of
            ExitFailure y -> Test.QuickCheck.Monadic.assert False 
            _             -> Test.QuickCheck.Monadic.assert True

main = quickCheckWith stdArgs { maxSuccess = 50000, maxSize = 500 } prop 
