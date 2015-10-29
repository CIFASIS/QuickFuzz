{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Png where
import Args
import Test.QuickCheck
import Check

import Codec.Picture.Types
import Codec.Picture.Png
import Codec.Picture.Png.Type
import Codec.Picture.Png.Export
import Codec.Picture.Metadata

import qualified Data.ByteString.Lazy as L

--import GHC.Types
--import GHC.Word

import Vector
import Images

import DeriveArbitrary

import Data.List.Split

--fromRight  :: Either a b -> b
--fromRight (Right x)  = x
--fromRight (Left x) = error "abc"

$(deriveArbitraryRec ''PngImageType)

type MPngImage = (Maybe Palette, PngImageType, Metadatas, Image Pixel8) --(Metadatas, PngImageType, Maybe Palette, Image Pixel8)

encodePngImage :: MPngImage -> L.ByteString
encodePngImage (a,b,c,d) = (genericEncodePng a b c d) --(encodePalettedPngWithMetadata a b c)

mencode :: MPngImage -> L.ByteString
mencode = encodePngImage

pngmain (MainArgs _ cmd filename prop maxSuccess maxSize outdir b) = let (prog, args) = (head spl, tail spl) in
    (case prop of
        "zzuf" -> quickCheckWith stdArgs {chatty = not b, maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ zzufprop filename prog args mencode outdir)
        "check" -> quickCheckWith stdArgs {chatty = not b, maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ checkprop filename prog args mencode outdir)
        "gen" -> quickCheckWith stdArgs {chatty = not b, maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ genprop filename prog args mencode outdir)
        "exec" -> quickCheckWith stdArgs {chatty = not b, maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ execprop filename prog args mencode outdir)
        _     -> error "Invalid action selected"
    ) where spl = splitOn " " cmd

main fargs False = pngmain $ fargs ""
main fargs True  = processPar fargs pngmain
