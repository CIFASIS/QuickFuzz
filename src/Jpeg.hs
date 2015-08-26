{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Jpeg where

import Test.QuickCheck
import Check

--import Control.Monad.Zip
--import Control.Exception
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
import Data.Int( Int16, Int8 )

import DeriveArbitrary
import ByteString
import Vector
import Images 

import GHC.Types
import GHC.Word

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS

import System.Process
import System.Exit

type MJpgImage  = (Word8,Metadatas, Image PixelYCbCr8)
encodeJpgImage (quality, metas, img) = encodeJpegAtQualityWithMetadata quality metas img

mencode :: MJpgImage -> L.ByteString
mencode = encodeJpgImage

--main = quickCheckWith stdArgs { maxSuccess = 120, maxSize = 50 } (absprop "buggy_qc.jp2" "" [] mencode)
main = quickCheckWith stdArgs { maxSuccess = 20, maxSize = 20 } (noShrinking $ genprop "buggy_qc.jpeg" "./bins/gdk-pixbuf" ["buggy_qc.jpeg"] mencode "data/jpeg")

