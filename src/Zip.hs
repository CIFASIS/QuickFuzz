{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Zip where

import Args
import Test.QuickCheck
import Check
import DeriveArbitrary

import Data.Binary( Binary(..), encode )

import Codec.Archive.Zip

import qualified Data.ByteString.Lazy as L
import Data.Word(Word8, Word16, Word32)

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS

import Vector
import ByteString

import Data.List.Split

$(deriveArbitraryRec ''Archive)

mencode :: Archive -> L.ByteString
mencode = encode

process filename cmd prop maxSuccess maxSize outdir =
    let (prog, args) = (head spl, tail spl) in
        (case prop of
            "zzuf" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ zzufprop filename prog args mencode outdir)
            "check" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ checkprop filename prog args mencode outdir)
            "gen" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ genprop filename prog args mencode outdir)
            "exec" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ execprop filename prog args mencode outdir)
            _     -> error "Invalid action selected"
        ) where spl = splitOn " " cmd
    

main (MainArgs _ filename cmd prop maxSuccess maxSize outdir) = process filename cmd prop maxSuccess maxSize outdir
