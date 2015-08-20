{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Tiff where

import Test.QuickCheck
import Check

import Control.Monad.Zip
import Control.Exception
import Data.Binary( Binary(..), encode )

import Codec.Picture.Types
import Codec.Picture.Tiff
import Codec.Picture.Tiff.Types
import Codec.Picture.Types
import Codec.Picture.Metadata.Exif
import Codec.Picture.Metadata

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

import Data.DeriveTH
import Data.Word(Word8, Word16, Word32)
import Data.Int( Int16, Int8 )
import Data.Binary.Put( runPut )

import DeriveArbitrary

import GHC.Types
import GHC.Word

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS

import Codec.Picture.VectorByteConversion( toByteString )

import System.Process
import System.Exit

instance Arbitrary a => Arbitrary (V.Vector a) where
   arbitrary = do 
     l <- listOf arbitrary
     return $ V.fromList l

instance (VU.Unbox a, Arbitrary a) => Arbitrary (VU.Vector a) where
   arbitrary = do 
     l <- listOf arbitrary
     return $ VU.fromList l

instance (VS.Storable a, Arbitrary a) => Arbitrary (VS.Vector a) where
   arbitrary = do 
     l <- listOf arbitrary
     return $ VS.fromList l


instance Arbitrary B.ByteString where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Word8)
     return $ B.pack l

instance Arbitrary L.ByteString where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Word8)
     return $ L.pack l

instance Arbitrary (Metadatas) where
  arbitrary = do
      w <- (arbitrary :: Gen Word)
      s <- (arbitrary :: Gen String)
      d <- (arbitrary :: Gen Double) 
      sf <- (arbitrary :: Gen SourceFormat)
      return $ Metadatas { getMetadatas = [ Format :=> sf, Gamma :=> d,  DpiX :=> w, DpiY :=> w, Width :=> w, Height :=> w, Title :=> s] }

instance Arbitrary (Image PixelYCbCr8) where
   arbitrary = do
       l <- listOf (arbitrary :: Gen (PixelBaseComponent PixelYCbCr8))
       w <- (arbitrary :: Gen Int)
       h <- (arbitrary :: Gen Int)
       return $ Image { imageWidth = w, imageHeight = h, imageData = VS.fromList [1] }

instance Arbitrary (Image PixelRGB8) where
   arbitrary = do
       l <- listOf (arbitrary :: Gen (PixelBaseComponent PixelRGB8))
       w <- (arbitrary :: Gen Int)
       h <- (arbitrary :: Gen Int)
       return $ Image { imageWidth = w, imageHeight = h, imageData = VS.fromList [1] }


instance Arbitrary (Image PixelRGB16) where
   arbitrary = do
       l <- listOf (arbitrary :: Gen (PixelBaseComponent PixelRGB16))
       w <- (arbitrary :: Gen Int)
       h <- (arbitrary :: Gen Int)
       return $ Image { imageWidth = w, imageHeight = h, imageData = VS.fromList [1] }


instance Show (Image PixelYCbCr8) where
   show x = ""

instance Show (Image PixelRGB8) where
   show x = ""

instance Show (Image PixelRGB16) where
   show x = ""


type TiffFile  = (TiffInfo, Image PixelRGB8)

encodeTiffFile :: TiffFile -> L.ByteString 
encodeTiffFile (hdr,img) = runPut $ putP rawPixelData hdr
                           where rawPixelData = toByteString $ imageData img

--derive makeArbitrary ''TiffFile
derive makeArbitrary ''TiffInfo
derive makeShow ''TiffInfo


--derive makeArbitrary ''Metadatas
derive makeArbitrary ''Predictor
derive makeShow ''Predictor

derive makeArbitrary ''ExtraSample
derive makeShow ''ExtraSample

derive makeArbitrary ''TiffCompression
derive makeShow ''TiffCompression

derive makeArbitrary ''TiffSampleFormat
derive makeShow ''TiffSampleFormat

derive makeArbitrary ''TiffPlanarConfiguration
derive makeArbitrary ''TiffColorspace
derive makeArbitrary ''TiffHeader
derive makeArbitrary ''Endianness
derive makeArbitrary ''SourceFormat

derive makeShow ''TiffPlanarConfiguration
derive makeShow ''TiffColorspace



-- $(deriveArbitraryRec ''TiffFile)

mencode :: TiffFile -> L.ByteString
mencode = encodeTiffFile

main = quickCheckWith stdArgs { maxSuccess = 120000000, maxSize = 10 } (absprop "buggy_qc.tiff" "bins/pixbuf_vuln_poc" ["buggy_qc.tiff"] mencode)
--main = quickCheckWith stdArgs { maxSuccess = 1200, maxSize = 50 } (genprop "buggy_qc.jp2" "" [] mencode "data/jpeg")

