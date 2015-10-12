{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}
{-# OPTIONS_GHC -threaded -rtsopts #-}

module Gif where

import Args
import Test.QuickCheck
import DeriveArbitrary
import Check

import Control.Exception
import Data.Binary( Binary(..), encode )

import Codec.Picture.Gif
import Codec.Picture.Types

import Vector
import ByteString
import Images

import Parallel

import qualified Data.ByteString.Lazy as L
import Data.DeriveTH

--import qualified Data.Vector as V
--import qualified Data.Vector.Unboxed as VU
--import qualified Data.Vector.Storable as VS


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

--handler :: SomeException -> IO ()
--handler _ = return ()

--fromRight           :: Either a b -> b
fromRight (Right x)  = x
fromRight (Left x) = error x

type MGifImage  = (GifLooping, [(Palette, GifDelay, Image Pixel8)])
encodeMGifImage :: MGifImage -> L.ByteString
encodeMGifImage (a, b) = fromRight $ encodeGifImages a b--encodeGifImageWithPalette img pal

mencode :: MGifImage -> L.ByteString
mencode = encodeMGifImage

process :: FilePath -> String -> String -> Int -> Int -> FilePath -> IO ()
process filename cmd prop maxSuccess maxSize outdir =
    let (prog, args) = (head spl, tail spl)
    in (case prop of
        "zzuf" ->
            quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize }
            (noShrinking $ zzufprop filename prog args mencode outdir)
        "radamsa" ->
            quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize }
            (noShrinking $ radamprop filename prog args mencode outdir)
        "check" ->
            quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize }
            (noShrinking $ checkprop filename prog args mencode outdir)
        "gen" ->
            quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize }
            (noShrinking $ genprop filename prog args mencode outdir)
        "exec" ->
            quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize }
            (noShrinking $ execprop filename prog args mencode outdir)
        _     -> error "Invalid action selected"
    ) where spl = splitOn " " cmd


main (MainArgs _ filename cmd prop maxSuccess maxSize outdir) = processIN filename cmd prop maxSuccess maxSize outdir process
