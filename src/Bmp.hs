{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}
module Bmp where

import Images
import Test.QuickCheck

import Data.Binary( Binary(..), encode )
import Codec.Picture.Bitmap
import Codec.Picture.Types
import Codec.Picture.Metadata

import qualified Data.ByteString.Lazy as L

import Data.DeriveTH

type BmpFile  = (BmpPalette, Image PixelRGBA8)

derive makeArbitrary ''BmpPalette
derive makeArbitrary ''BmpInfoHeader
derive makeArbitrary ''BmpHeader

derive makeShow ''BmpPalette
derive makeShow ''BmpHeader

encodeBMPFile :: BmpFile -> L.ByteString
encodeBMPFile (pal,img) = encodeBitmapWithPalette pal img

mencode = encodeBMPFile
