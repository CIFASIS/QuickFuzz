{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module Gif where

import Test.QuickCheck
import Data.Binary( Binary(..), encode, decode )

import Codec.Picture.Gif
import Codec.Picture.Types
import Codec.Picture.ColorQuant


import Mutation
import DeriveMutation

import DeriveArbitrary hiding (derive)
import ByteString
import Vector
import Images

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Data.DeriveTH


-- $(deriveArbitraryRec ''GifImage)
-- $(deriveArbitraryRec ''GifLooping)
-- $(deriveArbitraryRec ''GifFile)

--derive makeShow ''GifImage

instance Show (GifImage) where
   show x = "(GifImage)"

derive makeShow ''GraphicControlExtension
derive makeShow ''GifHeader
derive makeShow ''DisposalMethod
derive makeShow ''LogicalScreenDescriptor
derive makeShow ''GifVersion
derive makeShow ''ImageDescriptor
derive makeShow ''GifLooping
derive makeShow ''GifFile

fromRight (Right x)  = x
fromRight (Left x) = error x

--data MGifImage  = Gif0 GifFile | Gif1 (GifLooping, [(GifDelay, Image PixelRGB8)]) | Gif2 (Image Pixel8, Palette) deriving Show

type MGifImage = GifFile

$(devArbitrary ''MGifImage)
-- $(devMutationRec ''MGifImage)

-- $(devArbitrary ''GifLooping)

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
