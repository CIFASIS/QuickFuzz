{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module Gif where

import Test.QuickCheck
import DeriveArbitrary
import Check

--import Control.Monad.Zip
import Control.Exception
import Data.Binary( Binary(..), encode )

import Codec.Picture.Gif
import Codec.Picture.Types

import Vector
import ByteString
import Images

import qualified Data.ByteString.Lazy as L
import Data.DeriveTH
--import Data.Int( Int16, Int8 )

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS

--import Control.Monad.Reader()

import Data.List.Split

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

handler :: SomeException -> IO ()
handler _ = return ()

--fromRight           :: Either a b -> b
fromRight (Right x)  = x
fromRight (Left x) = error x

type MGifImage  = (GifLooping, [(Palette, GifDelay, Image Pixel8)])
encodeMGifImage :: MGifImage -> L.ByteString
encodeMGifImage (a, b) = fromRight $ encodeGifImages a b--encodeGifImageWithPalette img pal

mencode :: MGifImage -> L.ByteString
mencode = encodeMGifImage

--main = quickCheckWith stdArgs { maxSuccess = 120000000, maxSize = 25 } (noShrinking $ fuzzprop "buggy_qc.gif" "/usr/bin/giftopnm" ["buggy_qc.gif"] mencode)
--main = quickCheckWith stdArgs { maxSuccess = 1200000, maxSize = 20 } (noShrinking $ fuzzprop "buggy_qc.gif" "/usr/bin/gifrsize" ["-q-", "-S", "128", "128", "buggy_qc.gif"]  mencode)

--main = quickCheckWith stdArgs { maxSuccess = 1200000, maxSize = 20 } (noShrinking $ fuzzprop "buggy_qc.gif" "/usr/bin/convert.im6" ["buggy_qc.gif", "-resize", "128x128", "png:-"]  mencode)

main filename cmd prop maxSuccess maxSize = let (prog, args) = (head spl, tail spl) in
    (case prop of
        "fuzz" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ fuzzprop filename prog args mencode)
        "check" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ checkprop filename prog args mencode)
        "gen" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ genprop filename prog args mencode)
    ) where spl = splitOn " " cmd
