{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module Gif where

import Test.QuickCheck
import Data.Binary( Binary(..), encode, decode )

import Codec.Picture.Gif
import Codec.Picture.Types
import Codec.Picture.ColorQuant


import Mutation
import DeriveMutation

import Megadeth.DeriveShow

import DeriveArbitrary hiding (derive)
import ByteString
import Vector
import Images

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Data.DeriveTH


fromRight (Right x)  = x
fromRight (Left x) = error x

--data MGifImage  = Gif0 GifFile | Gif1 (GifLooping, [(GifDelay, Image PixelRGB8)]) | Gif2 (Image Pixel8, Palette) deriving Show

type MGifImage = GifFile

$(devShow ''GifFile)
$(devArbitrary ''MGifImage)

encodeMGifImage :: MGifImage -> L.ByteString
encodeMGifImage x = encode x

--encodeMGifImage (Gif0 x) = encode x
--encodeMGifImage (Gif1 (a, xs)) = fromRight $ encodeGifImages a (paletizes xs)
----encodeMGifImage (Gif2 (a, p))  = fromRight $ encodeGifImageWithPalette a p

paletizes [] = []
paletizes ((delay,img):xs) = (pal, delay, img'):(paletizes xs)
                              where (img', pal) = palettize defaultPaletteOptions img
                             
mencode :: MGifImage -> L.ByteString
mencode = encodeMGifImage

mdecode :: B.ByteString -> MGifImage
mdecode x = decode (L.pack (B.unpack x))
