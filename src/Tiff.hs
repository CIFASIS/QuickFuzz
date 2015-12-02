{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Tiff where

import Test.QuickCheck

import Data.Binary( Binary(..), encode )

import Codec.Picture.Types
import Codec.Picture.Tiff
import Codec.Picture.Tiff.Types
import Codec.Picture.Types
import Codec.Picture.Metadata.Exif
import Codec.Picture.Metadata

import qualified Data.ByteString.Lazy as L

import Data.DeriveTH
--import Data.Binary.Put( runPut )

import DeriveArbitrary
import Vector
import Images

--import Codec.Picture.VectorByteConversion( toByteString )

type TiffFile  = (Image PixelCMYK16)

encodeTiffFile :: TiffFile -> L.ByteString
encodeTiffFile = encodeTiff--runPut $ putP rawPixelData hdr
                       --    where rawPixelData = toByteString $ imageData img

derive makeArbitrary ''TiffInfo
derive makeShow ''TiffInfo


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

derive makeShow ''TiffPlanarConfiguration
derive makeShow ''TiffColorspace

mencode :: TiffFile -> L.ByteString
mencode = encodeTiffFile
