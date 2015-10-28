{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Jpeg where

import Args
import Test.QuickCheck
import Check

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

--import qualified Data.Vector as V
--import qualified Data.Vector.Unboxed as VU
--import qualified Data.Vector.Storable as VS

--import System.Process
--import System.Exit

import Data.List.Split

--import Codec.Picture.Jpg.Types 

$(deriveArbitraryRec ''JpgImage)

type MJpgImage  = JpgImage --(Word8,Metadatas, Image PixelYCbCr8)
--encodeJpgImage (quality, metas, img) = encodeJpegAtQualityWithMetadata quality metas img
encodeJpgImage = encode

mencode :: MJpgImage -> L.ByteString
mencode = encodeJpgImage

main (MainArgs _ cmd filename prop maxSuccess maxSize outdir _) = let (prog, args) = (head spl, tail spl) in
    (case prop of
        "zzuf" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ zzufprop filename prog args mencode outdir)
        "check" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ checkprop filename prog args mencode outdir)
        "gen" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ genprop filename prog args mencode outdir)
        "exec" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ execprop filename prog args mencode outdir)
        _     -> error "Invalid action selected"
    ) where spl = splitOn " " cmd
