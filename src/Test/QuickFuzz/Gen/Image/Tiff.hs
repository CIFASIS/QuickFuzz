{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.QuickFuzz.Gen.Image.Tiff where

import Data.Default

import qualified Data.Binary
import Data.Binary.Put( runPut )

import Codec.Picture.Tiff
import Codec.Picture.Tiff.Types
import Codec.Picture.Types
import Codec.Picture.Metadata.Exif
import Codec.Picture.Metadata
import Codec.Picture.VectorByteConversion( toByteString )

import Test.QuickCheck
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.List
import Data.Monoid 

import Test.QuickFuzz.Derive.Arbitrary

import Test.QuickFuzz.Derive.Show
import Test.QuickFuzz.Gen.FormatInfo
import Test.QuickFuzz.Gen.Base.ByteString
import Test.QuickFuzz.Gen.Base.String
import Test.QuickFuzz.Gen.Base.Image
import Test.QuickFuzz.Gen.Base.Vector

import qualified Data.ByteString.Lazy as L

data TiffImage =   Tiff0 (TiffInfo, Image Pixel8) 
                   -- | Tiff1 (Image PixelCMYK16) 
                   -- | Tiff2 (Image PixelYA8) 
                   -- | Tiff3 (Image PixelRGBA8) 
                   -- | Tiff4 (Image PixelYCbCr8)


instance Arbitrary TiffHeader where
  arbitrary = return $ TiffHeader {hdrEndianness = EndianBig, hdrOffset = 0}

devArbitrary ''TiffImage
devShow ''TiffImage

tiffencode :: TiffImage -> L.ByteString
tiffencode (Tiff0 (hdr, img)) = runPut $ putP rawPixelData hdr
                                  where rawPixelData = toByteString $ imageData img

--tiffencode (Tiff1 img) = encodeTiff img
--tiffencode (Tiff2 img) = encodeTiff img
--tiffencode (Tiff3 img) = encodeTiff img
--tiffencode (Tiff4 img) = encodeTiff img

tiffInfo :: FormatInfo TiffImage NoActions
tiffInfo = def 
    { encode = tiffencode
    , random = arbitrary
    , value = show
    , ext = "tiff" 
    } 
