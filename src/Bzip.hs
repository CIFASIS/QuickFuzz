{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Bzip where

import Test.QuickCheck

import Codec.Compression.BZip.Internal

import qualified Data.ByteString.Lazy as L

import Data.DeriveTH
import DeriveArbitrary hiding (derive)

import ByteString

type MBzipFile  = (CompressParams,L.ByteString)

$(deriveArbitrary ''MBzipFile)
derive makeShow ''CompressParams
derive makeShow ''WorkFactor
derive makeShow ''BlockSize

mencode :: MBzipFile -> L.ByteString
mencode (p,bs) = compress p bs
