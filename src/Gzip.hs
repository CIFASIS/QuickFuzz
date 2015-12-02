{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Gzip where

import Test.QuickCheck
import Codec.Compression.GZip
--import Data.Binary( Binary(..), encode )

import qualified Data.ByteString.Lazy as L

import Data.DeriveTH
import DeriveArbitrary
import ByteString

type MGzipFile  = (CompressParams,L.ByteString)

$(deriveArbitraryRec ''CompressParams)

mencode :: MGzipFile -> L.ByteString
mencode (p,bs) = compressWith p bs
