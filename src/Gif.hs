{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

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

process :: Bool -> FilePath -> String -> String -> Int -> Int -> FilePath -> IO Result 
process par filename cmd prop maxSuccess maxSize outdir =
    let (prog, args) = (head spl, tail spl)
    in (case prop of
        "zzuf" ->
            quickCheckWithResult stdArgs { maxSuccess = maxSuccess , maxSize = maxSize, chatty = not par }
            (noShrinking $ zzufprop filename prog args mencode outdir)
        "radamsa" ->
            quickCheckWithResult stdArgs { maxSuccess = maxSuccess , maxSize = maxSize, chatty = not par }
            (noShrinking $ radamprop filename prog args mencode outdir)
        "check" ->
            quickCheckWithResult stdArgs { maxSuccess = maxSuccess , maxSize = maxSize, chatty = not par }
            (noShrinking $ checkprop filename prog args mencode outdir)
        "gen" ->
            quickCheckWithResult stdArgs { maxSuccess = maxSuccess , maxSize = maxSize, chatty = not par }
            (noShrinking $ genprop filename prog args mencode outdir)
        "exec" ->
            quickCheckWithResult stdArgs { maxSuccess = maxSuccess , maxSize = maxSize, chatty = not par }
            (noShrinking $ execprop filename prog args mencode outdir)
        _     -> error "Invalid action selected"
    ) where spl = splitOn " " cmd

gifmain (MainArgs _ cmd filename prop maxSuccess maxSize outdir b) = process b filename cmd prop maxSuccess maxSize outdir 

main fargs False = (\x -> (gifmain x) >> return ()) $ fargs ""
main fargs True  = processPar fargs (\x -> (gifmain x) >> return ())
