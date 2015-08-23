{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}
module Bmp where

import Check
import Images
import Test.QuickCheck

import Data.Binary( Binary(..), encode )
import Codec.Picture.Bitmap
import Codec.Picture.Types
import Codec.Picture.Metadata

import qualified Data.ByteString.Lazy as L

import Data.DeriveTH
import DeriveArbitrary

import Data.Binary.Put( runPut )

type BmpFile  = (BmpHeader, BmpInfoHeader, BmpPalette, Image PixelRGBA8)

-- $(deriveArbitraryRec ''BmpFile)

derive makeArbitrary ''SourceFormat
derive makeArbitrary ''BmpPalette
derive makeArbitrary ''BmpInfoHeader
derive makeArbitrary ''BmpHeader

derive makeShow ''BmpPalette
derive makeShow ''BmpHeader

encodeBMPFile :: BmpFile -> L.ByteString
encodeBMPFile (hdr, info, pal, img) = runPut $ put hdr >> put info >> putPalette pal >> bmpEncode img

main = quickCheckWith stdArgs { maxSuccess = 50000, maxSize = 100 } (checkprop "buggy_qc.bmp" "bins/pixbuf_vuln_poc" ["buggy_qc.bmp"] encodeBMPFile)
