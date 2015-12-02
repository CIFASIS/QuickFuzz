{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module Gif where

import DeriveArbitrary

import Data.Binary( Binary(..), encode )

import Codec.Picture.Gif
import Codec.Picture.Types

import Vector
import ByteString
import Images

import qualified Data.ByteString.Lazy as L
import Data.DeriveTH

$(deriveArbitraryRec ''GifImage)
$(deriveArbitraryRec ''GifLooping)

derive makeShow ''GifImage
derive makeShow ''GraphicControlExtension
derive makeShow ''GifHeader
derive makeShow ''DisposalMethod
derive makeShow ''LogicalScreenDescriptor
derive makeShow ''GifVersion
derive makeShow ''ImageDescriptor
derive makeShow ''GifLooping

fromRight (Right x)  = x
fromRight (Left x) = error x

type MGifImage  = (GifLooping, [(Palette, GifDelay, Image Pixel8)])
encodeMGifImage :: MGifImage -> L.ByteString
encodeMGifImage (a, b) = fromRight $ encodeGifImages a b

mencode :: MGifImage -> L.ByteString
mencode = encodeMGifImage
