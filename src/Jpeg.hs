{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Jpeg where

import Test.QuickCheck

import Data.Binary( Binary(..), encode )

import Codec.Picture.Types
import Codec.Picture.Jpg
import Codec.Picture.Jpg.Types
import Codec.Picture.Tiff.Types
import Codec.Picture.Jpg.DefaultTable
import Codec.Picture.Metadata.Exif
import Codec.Picture.Metadata

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

import Data.DeriveTH
import Data.Word(Word8, Word16, Word32)

import DeriveArbitrary
import ByteString
import Vector
import Images

$(deriveArbitraryRec ''JpgImage)

data MJpgImage  = Jpg0 JpgImage | Jpg1 (Word8,Metadatas, Image PixelYCbCr8) | Jpg2 (Word8, Image PixelYCbCr8) deriving Show

derive makeArbitrary ''MJpgImage

--type MJpgImage  = (Word8, Metadatas, Image PixelYCbCr8)

--encodeJpgImage (q, meta, img) = encodeJpegAtQualityWithMetadata q meta img
--encodeJpgImage (q, _, img) = encodeJpegAtQuality q img
encodeJpgImage (Jpg0 x) = encode x
encodeJpgImage (Jpg1 (q, meta, img)) = encodeJpegAtQualityWithMetadata q meta img
encodeJpgImage (Jpg2 (q, img)) = encodeJpegAtQuality q img


mencode :: MJpgImage -> L.ByteString
mencode = encodeJpgImage
