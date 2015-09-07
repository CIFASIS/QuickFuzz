{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Bzip where

import Test.QuickCheck
import Check


--import Codec.Compression.GZip
import Codec.Compression.BZip.Internal

import Data.Binary( Binary(..), encode )
import Data.Word(Word8, Word16, Word32)

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

main filename cmd prop maxSuccess maxSize = let (prog, args) = (head spl, tail spl) in
    (case prop of
        "fuzz" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ fuzzprop filename prog args mencode)
        "check" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ checkprop filename prog args mencode)
        "gen" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ genprop filename prog args mencode)
    ) where spl = splitOn " " cmd

--main = quickCheckWith stdArgs { maxSuccess = 120, maxSize = 50 } (absprop "buggy_qc.jp2" "" [] mencode)
--main = quickCheckWith stdArgs { maxSuccess = 200000, maxSize = 100 } (noShrinking $ fuzzprop "buggy_qc.gz" "/bin/gzip" ["-d", "--to-stdout", "buggy_qc.gz"] mencode)

