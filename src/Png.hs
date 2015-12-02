{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Png where

import Test.QuickCheck
import Codec.Picture.Types
import Codec.Picture.Png
import Codec.Picture.Png.Type
import Codec.Picture.Png.Export
import Codec.Picture.Metadata

import qualified Data.ByteString.Lazy as L

import Vector
import Images
import DeriveArbitrary

$(deriveArbitraryRec ''PngImageType)

type MPngImage = (Maybe Palette, PngImageType, Metadatas, Image Pixel8) --(Metadatas, PngImageType, Maybe Palette, Image Pixel8)

encodePngImage :: MPngImage -> L.ByteString
encodePngImage (a,b,c,d) = (genericEncodePng a b c d) --(encodePalettedPngWithMetadata a b c)

mencode :: MPngImage -> L.ByteString
mencode = encodePngImage
