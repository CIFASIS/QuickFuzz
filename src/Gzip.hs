{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Gzip where

import Test.QuickCheck
import Check


import Codec.Compression.GZip

import Data.Binary( Binary(..), encode )
import Data.Word(Word8, Word16, Word32)

import qualified Data.ByteString.Lazy as L

import Data.DeriveTH
import DeriveArbitrary

import ByteString

type MGzipFile  = (CompressParams,L.ByteString)

$(deriveArbitraryRec ''CompressParams)

mencode :: MGzipFile -> L.ByteString
mencode (p,bs) = compressWith p bs

--main = quickCheckWith stdArgs { maxSuccess = 120, maxSize = 50 } (absprop "buggy_qc.jp2" "" [] mencode)
main = quickCheckWith stdArgs { maxSuccess = 200000, maxSize = 100 } (noShrinking $ fuzzprop "buggy_qc.gz" "/bin/gzip" ["-d", "--to-stdout", "buggy_qc.gz"] mencode)

