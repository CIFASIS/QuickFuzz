{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Gzip where

import Args
import Test.QuickCheck
import Check


import Codec.Compression.GZip
import Data.Binary( Binary(..), encode )
--import Data.Word(Word8, Word16, Word32)

import qualified Data.ByteString.Lazy as L

import Data.DeriveTH
import DeriveArbitrary
import Data.List.Split
import ByteString

type MGzipFile  = (CompressParams,L.ByteString)

$(deriveArbitraryRec ''CompressParams)

mencode :: MGzipFile -> L.ByteString
mencode (p,bs) = compressWith p bs

main (MainArgs _ filename cmd prop maxSuccess maxSize outdir) = let (prog, args) = (head spl, tail spl) in
    (case prop of
        "zzuf" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ zzufprop filename prog args mencode outdir)
        "check" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ checkprop filename prog args mencode outdir)
        "gen" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ genprop filename prog args mencode outdir)
        "exec" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ execprop filename prog args mencode outdir)
        _     -> error "Invalid action selected"
    ) where spl = splitOn " " cmd
