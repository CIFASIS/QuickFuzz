{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Png where

import Test.QuickCheck
import Check

import Codec.Picture.Types
import Codec.Picture.Png
import Codec.Picture.Png.Type
import Codec.Picture.Png.Export
import Codec.Picture.Metadata

import qualified Data.ByteString.Lazy as L

import GHC.Types
import GHC.Word

import Vector
import Images

import DeriveArbitrary

import Data.List.Split

fromRight  :: Either a b -> b
fromRight (Right x)  = x
fromRight (Left x) = error "abc"

$(deriveArbitraryRec ''PngImageType)

type MPngImage = (Maybe Palette, PngImageType, Metadatas, Image Pixel8) --(Metadatas, PngImageType, Maybe Palette, Image Pixel8)

encodePngImage :: MPngImage -> L.ByteString
encodePngImage (a,b,c,d) = (genericEncodePng a b c d) --(encodePalettedPngWithMetadata a b c)

mencode :: MPngImage -> L.ByteString
mencode = encodePngImage

main filename cmd prop maxSuccess maxSize = let (prog, args) = (head spl, tail spl) in
    (case prop of
        "fuzz" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ fuzzprop filename prog args mencode)
        "check" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ checkprop filename prog args mencode)
        "gen" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ genprop filename prog args mencode)
    ) where spl = splitOn " " cmd
