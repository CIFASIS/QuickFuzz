{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Tiff where

import Test.QuickCheck

import Data.Binary( Binary(..), encode )

import Codec.Picture.Types
import Codec.Picture.Tiff
import Codec.Picture.Tiff.Types
import Codec.Picture.Metadata.Exif
import Codec.Picture.Metadata

import qualified Data.ByteString.Lazy as L

import Data.DeriveTH
import Data.Binary.Put( runPut )

import DeriveArbitrary hiding (derive)
import DeriveShow

import Vector
import Images

import Codec.Picture.VectorByteConversion( toByteString )

derive makeArbitrary ''TiffInfo

data MTiffFile  = Tiff0 (TiffInfo, Image Pixel8) | Tiff1 (Image PixelCMYK16) | Tiff2 (Image PixelYA8) | Tiff3 (Image PixelRGBA8) | Tiff4 (Image PixelYCbCr8)

derive makeArbitrary ''MTiffFile
$(devShow ''MTiffFile)


encodeTiffFile :: MTiffFile -> L.ByteString
encodeTiffFile (Tiff0 (hdr, img)) = runPut $ putP rawPixelData hdr
                                  where rawPixelData = toByteString $ imageData img

encodeTiffFile (Tiff1 img) = encodeTiff img
encodeTiffFile (Tiff2 img) = encodeTiff img
encodeTiffFile (Tiff3 img) = encodeTiff img
encodeTiffFile (Tiff4 img) = encodeTiff img

derive makeArbitrary ''Predictor
derive makeArbitrary ''ExtraSample
derive makeArbitrary ''TiffCompression
derive makeArbitrary ''TiffSampleFormat
derive makeArbitrary ''TiffPlanarConfiguration
derive makeArbitrary ''TiffColorspace
derive makeArbitrary ''TiffHeader
derive makeArbitrary ''Endianness

mencode :: MTiffFile -> L.ByteString
mencode = encodeTiffFile
