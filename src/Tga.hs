{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Tga where

import Test.QuickCheck
import Data.Binary( Binary(..), encode )

import Codec.Picture.Types
import Codec.Picture.Tga.Types
import Codec.Picture.Metadata.Exif
import Codec.Picture.Metadata

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

import Data.DeriveTH
import DeriveArbitrary

import Megadeth.DeriveShow

import ByteString
import Images

data MTgaFile = Tga0 TgaFile_t | Tga1 (Image Pixel8) | Tga2 (Image PixelRGB8) | Tga3 (Image PixelRGBA8) 

$(devArbitrary ''MTgaFile)
$(devShow ''MTgaFile)

mencode :: MTgaFile -> L.ByteString
mencode (Tga0 x) = encode x
mencode (Tga1 x) = encodeTga x
mencode (Tga2 x) = encodeTga x
mencode (Tga3 x) = encodeTga x
