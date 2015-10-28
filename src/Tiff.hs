{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Tiff where

import Args
import Test.QuickCheck
import Check

import Control.Exception
import Data.Binary( Binary(..), encode )

import Codec.Picture.Types
import Codec.Picture.Tiff
import Codec.Picture.Tiff.Types
import Codec.Picture.Types
import Codec.Picture.Metadata.Exif
import Codec.Picture.Metadata

import qualified Data.ByteString.Lazy as L

import Data.DeriveTH
import Data.Binary.Put( runPut )

import DeriveArbitrary
import Vector
import Images

import Codec.Picture.VectorByteConversion( toByteString )

import Data.List.Split

type TiffFile  = (Image PixelCMYK16)

encodeTiffFile :: TiffFile -> L.ByteString
encodeTiffFile = encodeTiff--runPut $ putP rawPixelData hdr
                       --    where rawPixelData = toByteString $ imageData img

--derive makeArbitrary ''TiffFile
derive makeArbitrary ''TiffInfo
derive makeShow ''TiffInfo


--derive makeArbitrary ''Metadatas
derive makeArbitrary ''Predictor
derive makeShow ''Predictor

derive makeArbitrary ''ExtraSample
derive makeShow ''ExtraSample

derive makeArbitrary ''TiffCompression
derive makeShow ''TiffCompression

derive makeArbitrary ''TiffSampleFormat
derive makeShow ''TiffSampleFormat

derive makeArbitrary ''TiffPlanarConfiguration
derive makeArbitrary ''TiffColorspace
derive makeArbitrary ''TiffHeader
derive makeArbitrary ''Endianness

derive makeShow ''TiffPlanarConfiguration
derive makeShow ''TiffColorspace

-- $(deriveArbitraryRec ''TiffFile)

mencode :: TiffFile -> L.ByteString
mencode = encodeTiffFile

main (MainArgs _ cmd filename prop maxSuccess maxSize outdir _) = let (prog, args) = (head spl, tail spl) in
    (case prop of
        "zzuf" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ zzufprop filename prog args mencode outdir)
        "check" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ checkprop filename prog args mencode outdir)
        "gen" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ genprop filename prog args mencode outdir)
        "exec" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ execprop filename prog args mencode outdir)
        _     -> error "Invalid action selected"
    ) where spl = splitOn " " cmd
