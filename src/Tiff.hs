{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Tiff where

import Test.QuickCheck
import Check

--import Control.Monad.Zip
import Control.Exception
import Data.Binary( Binary(..), encode )

import Codec.Picture.Types
import Codec.Picture.Tiff
import Codec.Picture.Tiff.Types
import Codec.Picture.Types
import Codec.Picture.Metadata.Exif
import Codec.Picture.Metadata

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

import Data.DeriveTH
--import Data.Word(Word8, Word16, Word32)
--import Data.Int( Int16, Int8 )
import Data.Binary.Put( runPut )

import DeriveArbitrary
import Vector
import Images

--import GHC.Types
--import GHC.Word

import Codec.Picture.VectorByteConversion( toByteString )

type TiffFile  = (TiffInfo, Image PixelRGB8)

encodeTiffFile :: TiffFile -> L.ByteString 
encodeTiffFile (hdr,img) = runPut $ putP rawPixelData hdr
                           where rawPixelData = toByteString $ imageData img

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

main = quickCheckWith stdArgs { maxSuccess = 120000000, maxSize = 20 } (fuzzprop "buggy_qc.tif" "tiff2pdf" ["buggy_qc.tif"] mencode)
--main = quickCheckWith stdArgs { maxSuccess = 1200, maxSize = 50 } (genprop "buggy_qc.jp2" "" [] mencode "data/jpeg")

