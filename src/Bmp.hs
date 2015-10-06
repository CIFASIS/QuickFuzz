{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}
module Bmp where

import Args
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
import Data.List.Split

type BmpFile  = (BmpHeader, BmpInfoHeader, BmpPalette, Image PixelRGBA8)

derive makeArbitrary ''BmpPalette
derive makeArbitrary ''BmpInfoHeader
derive makeArbitrary ''BmpHeader

derive makeShow ''BmpPalette
derive makeShow ''BmpHeader

encodeBMPFile :: BmpFile -> L.ByteString
encodeBMPFile (hdr, info, pal, img) = runPut $ put hdr >> put info >> putPalette pal >> bmpEncode img

mencode = encodeBMPFile

main (MainArgs _ filename cmd prop maxSuccess maxSize outdir) = let (prog, args) = (head spl, tail spl) in
    (case prop of
        "zzuf" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ zzufprop filename prog args mencode outdir)
        "check" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ checkprop filename prog args mencode outdir)
        "gen" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ genprop filename prog args mencode outdir)
        "exec" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ execprop filename prog args mencode outdir)
        _     -> error "Invalid action selected"
    ) where spl = splitOn " " cmd
