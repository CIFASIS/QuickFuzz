{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Png where

import Test.QuickCheck
import Codec.Picture.Types
import Codec.Picture.Png
import Codec.Picture.Png.Type
import Codec.Picture.Png.Export
import Codec.Picture.Metadata

import qualified Data.ByteString.Lazy as L

import Data.Binary( Binary(..), encode )

import Vector
import Images
import DeriveArbitrary
import ByteString
import Data.DeriveTH


--derive makeArbitrary ''ChunkSignature
--derive makeArbitrary ''PngRawImage

$(deriveArbitraryRec ''PngRawImage)

type MPngImage = PngRawImage

instance Show MPngImage where
   show x = "(no show)"

--type MPngImage = (Maybe Palette, PngImageType, Metadatas, Image Pixel8) --(Metadatas, PngImageType, Maybe Palette, Image Pixel8)
--type MPngImage = PngImageType --(Metadatas, Image PixelRGBA8)

encodePngImage :: MPngImage -> L.ByteString
--encodePngImage (a,b,c,d) = (genericEncodePng a b c d) --(encodePalettedPngWithMetadata a b c)
--encodePngImage (a,b) = encodePngWithMetadata a b
encodePngImage = encode

mencode :: MPngImage -> L.ByteString
mencode = encodePngImage
