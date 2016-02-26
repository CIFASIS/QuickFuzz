{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Png where

import Test.QuickCheck
import Codec.Picture.Types
import Codec.Picture.Png
import Codec.Picture.Png.Type
import Codec.Picture.Png.Export
import Codec.Picture.Metadata
import Codec.Picture.ColorQuant

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

import Data.Binary( Binary(..), encode, decode )

import Vector
import Images

import Mutation
import DeriveMutation


import DeriveArbitrary
import ByteString
import Data.DeriveTH


fromRight (Right x)  = x
fromRight (Left x) = error x


$(devArbitrary ''PngRawImage)
$(devMutationRec ''PngRawImage)

instance Show PngRawImage where
   show x = "(no show)"

--data MPngImage =   Png0 PngRawImage 
                -- | Png1 (Metadatas, PngImageType, Maybe Palette, Image Pixel8) 
                -- | Png2 (Metadatas, Palette, Image Pixel8) 
                -- | Png3 (Metadatas, Image PixelRGB8) deriving Show

type MPngImage =  PngRawImage 

--derive makeArbitrary ''MPngImage

encodePngImage :: MPngImage -> L.ByteString
encodePngImage x = encode x
--encodePngImage (Png1 (a,b,c,d)) = genericEncodePng c b a d
--encodePngImage (Png2 (a,b,c)) = fromRight $ encodePalettedPngWithMetadata a b c 
--encodePngImage (Png3 (a,b)) = fromRight $ encodePalettedPngWithMetadata a pal img'
--                              where (img', pal) = palettize defaultPaletteOptions b

 
mencode :: MPngImage -> L.ByteString
mencode = encodePngImage

mdecode :: B.ByteString -> MPngImage
mdecode x = decode (L.pack (B.unpack x))
