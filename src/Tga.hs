{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Tga where

import Args
import Test.QuickCheck
import Check

import Data.Binary( Binary(..), encode )

import Codec.Picture.Types
import Codec.Picture.Tga.Types
import Codec.Picture.Metadata.Exif
import Codec.Picture.Metadata

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

import Data.DeriveTH
import Data.Word(Word8, Word16, Word32)
import Data.Int( Int16, Int8 )

import DeriveArbitrary

import GHC.Types
import GHC.Word

import ByteString
import Data.List.Split

derive makeShow ''TgaFile_t
derive makeShow ''TgaImageType
derive makeShow ''TgaImageDescription
derive makeShow ''TgaColorMapType
derive makeShow ''TgaHeader

derive makeArbitrary ''TgaFile_t
$(deriveArbitraryRec ''TgaHeader)

mencode :: TgaFile_t -> L.ByteString
mencode = encode

tgamain (MainArgs _ cmd filename prop maxSuccess maxSize outdir _) = let (prog, args) = (head spl, tail spl) in
    (case prop of
        "zzuf" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ zzufprop filename prog args mencode outdir)
        "check" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ checkprop filename prog args mencode outdir)
        "gen" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ genprop filename prog args mencode outdir)
        "exec" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ execprop filename prog args mencode outdir)
        _     -> error "Invalid action selected"
    ) where spl = splitOn " " cmd


main fargs False = tgamain $ fargs ""
main fargs True  = processPar fargs tgamain
