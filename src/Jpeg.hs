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

--type MJpgImage  = JpgImage --(Word8,Metadatas, Image PixelYCbCr8)
type MJpgImage  = (Word8,Metadatas, Image PixelYCbCr8)

encodeJpgImage (quality, metas, img) = encodeJpegAtQualityWithMetadata quality metas img

mencode :: MJpgImage -> L.ByteString
mencode = encodeJpgImage
