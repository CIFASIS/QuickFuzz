{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}
module QuickOgg where

import Check
import Test.QuickCheck

import Data.Binary( Binary(..), encode )

import Codec.Container.Ogg.Page
import Codec.Container.Ogg.Granulepos
import Codec.Container.Ogg.Track
import Codec.Container.Ogg.MessageHeaders
import Codec.Container.Ogg.Granulerate
import Codec.Container.Ogg.ContentType

import qualified Data.ByteString.Lazy as L

import Data.DeriveTH
import Data.Word(Word8, Word16, Word32)
import Data.Int( Int16, Int8 )

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS

import GHC.Types

import Data.Binary.Put( runPut )


derive makeArbitrary ''OggPage
derive makeArbitrary ''Granulepos
derive makeArbitrary ''OggTrack
derive makeArbitrary ''Granulerate
derive makeArbitrary ''ContentType
--derive makeArbitrary ''Map

instance Arbitrary L.ByteString where
   arbitrary = do
     l <- listOf (arbitrary :: Gen Word8)
     return $ L.pack l

instance Arbitrary MessageHeaders where
   arbitrary = do
     s1 <- arbitrary :: Gen String
     s2 <- arbitrary :: Gen String
     return $ mhSingleton s1 s2

instance CoArbitrary L.ByteString where
   coarbitrary x = coarbitrary $ L.unpack x

{-
derive makeArbitrary ''SourceFormat
derive makeArbitrary ''BmpPalette
derive makeArbitrary ''BmpInfoHeader
derive makeArbitrary ''BmpHeader
derive makeShow ''BmpPalette
derive makeShow ''BmpHeader


instance Arbitrary (V.Vector Word32) where
   arbitrary = do
     l <- listOf (arbitrary :: Gen Word32)
     return $ V.fromList l

instance Arbitrary (Image PixelRGB8) where
   arbitrary = do
       l <- listOf (arbitrary :: Gen Word8)
       w <- (arbitrary :: Gen Int)
       h <- (arbitrary :: Gen Int)
       return $ Image { imageWidth = w, imageHeight = h, imageData = VS.fromList l }

instance Show (Image PixelRGB8) where
   show x = ""

instance Arbitrary (Image PixelRGBA8) where
   arbitrary = do
       l <- listOf (arbitrary :: Gen Word8)
       w <- (arbitrary :: Gen Int)
       h <- (arbitrary :: Gen Int)
       return $ Image { imageWidth = w, imageHeight = h, imageData = VS.fromList l }

instance Show (Image PixelRGBA8) where
   show x = ""

instance Arbitrary (Image PixelRGB16) where
   arbitrary = do
       l <- listOf (arbitrary :: Gen Word16)
       w <- (arbitrary :: Gen Int)
       h <- (arbitrary :: Gen Int)
       return $ Image { imageWidth = w, imageHeight = h, imageData = VS.fromList l }

type BMPFile  = (BmpHeader, BmpInfoHeader, BmpPalette, Image PixelRGBA8)
-}

main = quickCheckWith stdArgs { maxSuccess = 50000, maxSize = 10 } (absprop "buggy_qc.ogg" "/usr/bin/ogginfo" ["buggy_qc.ogg"] pageWrite)
