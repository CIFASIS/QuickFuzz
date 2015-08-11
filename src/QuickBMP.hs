{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}
module QuickBMP where

import Check
import Test.QuickCheck

import Data.Binary( Binary(..), encode )

import Codec.Picture.Bitmap
import Codec.Picture.Types

import Codec.Picture.Metadata

import qualified Data.ByteString.Lazy as L

import Data.DeriveTH
import Data.Word(Word8, Word16, Word32)
import Data.Int( Int16, Int8 )

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS

import GHC.Types

--import Control.Monad
--  ( liftM
--  )

import Data.Binary.Put( runPut )
import Control.Monad.Reader()


-- $( derive makeArbitrary ''BmpHeader, ''BmpInfoHeader )

derive makeArbitrary ''SourceFormat
derive makeArbitrary ''BmpPalette
derive makeArbitrary ''BmpInfoHeader
derive makeArbitrary ''BmpHeader

--derive makeShow ''SourceFormat
derive makeShow ''BmpPalette
--derive makeShow ''BmpInfoHeader
derive makeShow ''BmpHeader




instance Arbitrary (V.Vector Word32) where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Word32)
     return $ V.fromList l
{-
instance Arbitrary (Metadatas) where
  arbitrary = do --return $ Title :=> "abc"
      w <- (arbitrary :: Gen Word)
      s <- (arbitrary :: Gen String)
      d <- (arbitrary :: Gen Double) 
      sf <- (arbitrary :: Gen SourceFormat)
      return $ Metadatas { getMetadatas = [ Format :=> sf, Gamma :=> d,  DpiX :=> w, DpiY :=> w, Width :=> w, Height :=> w, Title :=> s] }
-}
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

{-
instance Arbitrary (Image pixel0) where
   arbitrary = do
       l <- listOf (arbitrary :: Gen pixel0)
       w <- (arbitrary :: Gen Int)
       h <- (arbitrary :: Gen Int)
       return $ Image { imageWidth = w, imageHeight = h, imageData = VS.fromList l }

instance Show (Image pixel0) where
   show x = ""
-}

--type BMPFile  = (Metadatas, BmpPalette, Image PixelRGB8)
type BMPFile  = (BmpHeader, BmpInfoHeader, BmpPalette, Image PixelRGBA8)

encodeBMPFile :: BMPFile -> L.ByteString
encodeBMPFile (hdr, info, pal, img) = runPut $ put hdr >> put info >> putPalette pal >> bmpEncode img
--encodeBMPFile x = L.empty


main = quickCheckWith stdArgs { maxSuccess = 50000, maxSize = 10 } (absprop "buggy_qc.bmp" "/usr/bin/identify.im6" ["buggy_qc.bmp"] encodeBMPFile)
