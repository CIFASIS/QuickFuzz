{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Jpeg where

import Test.QuickCheck

import Data.Binary( Binary(..), encode, decode )

import Codec.Picture.Types
import Codec.Picture.Jpg
import Codec.Picture.Jpg.Types
import Codec.Picture.Jpg.DefaultTable
import Codec.Picture.Tiff.Types
import Codec.Picture.Jpg.DefaultTable
import Codec.Picture.Metadata.Exif
import Codec.Picture.Metadata

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

import Data.DeriveTH
import Data.Word(Word8, Word16, Word32)

import DeriveMutation
import DeriveMArbitrary 
import DeriveArbitrary 
import ByteString
import Vector
import Images

$(devActions ["Codec.Picture.Jpg.DefaultTable"] ''HuffmanTree False [])
$(devArbitrary ''HuffmanTreeAction)
$(devArbitraryWithActions False ''HuffmanTree)

$(devActions ["Codec.Picture.Jpg.DefaultTable"] ''HuffmanPackedTree False [])
$(devArbitrary ''HuffmanPackedTreeAction)
$(devArbitraryWithActions False ''HuffmanPackedTree)

$(devActions ["Codec.Picture.Jpg.DefaultTable"] ''HuffmanTable False [])
$(devArbitrary ''HuffmanTableAction)
$(devArbitraryWithActions False ''HuffmanTable)


data MJpgImage  = Jpg0 JpgImage deriving Show -- | Jpg1 (Word8,Metadatas, Image PixelYCbCr8) | Jpg2 (Word8, Image PixelYCbCr8) deriving Show

$(devArbitrary ''MJpgImage)
$(devMutation ''MJpgImage)

encodeJpgImage (Jpg0 x) = encode x
--encodeJpgImage (Jpg1 (q, meta, img)) = encodeJpegAtQualityWithMetadata q meta img
--encodeJpgImage (Jpg2 (q, img)) = encodeJpegAtQuality q img


mencode :: MJpgImage -> L.ByteString
mencode = encodeJpgImage

mdecode :: B.ByteString -> MJpgImage
mdecode x = Jpg0 $ decode (L.pack (B.unpack x))
