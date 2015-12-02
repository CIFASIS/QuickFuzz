{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Bzip where

import Test.QuickCheck

import Codec.Compression.BZip.Internal

import qualified Data.ByteString.Lazy as L

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
