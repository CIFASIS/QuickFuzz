{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Bzip where

import Args
import Test.QuickCheck
import Check

import Codec.Compression.BZip.Internal

--import Data.Binary( Binary(..), encode )
--import Data.Word(Word8, Word16, Word32)

import qualified Data.ByteString.Lazy as L
import Data.List.Split

import Data.DeriveTH
import DeriveArbitrary

import ByteString

type MBzipFile  = (CompressParams,L.ByteString)

$(deriveArbitraryRec ''CompressParams)
derive makeShow ''CompressParams
derive makeShow ''WorkFactor
derive makeShow ''BlockSize

mencode :: MBzipFile -> L.ByteString
mencode (p,bs) = compress p bs

main (MainArgs _ cmd filename prop maxSuccess maxSize outdir _) = let (prog, args) = (head spl, tail spl) in
    (case prop of
        "zzuf" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ zzufprop filename prog args mencode outdir)
        "check" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ checkprop filename prog args mencode outdir)
        "gen" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ genprop filename prog args mencode outdir)
        "exec" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ execprop filename prog args mencode outdir)
        _     -> error "Invalid action selected"
    ) where spl = splitOn " " cmd
