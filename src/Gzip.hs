{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Gzip where

import Test.QuickCheck
import Codec.Compression.GZip
--import Data.Binary( Binary(..), encode )

import qualified Data.ByteString.Lazy as L

import Data.DeriveTH
import DeriveArbitrary
import ByteString

--type MGzipFile  = (CompressParams,L.ByteString)

data MGzipFile = { id1 :: Word8,
		   id2 :: Word8,
		   cm :: Word8,
		   flg :: Flag,
		   mtime :: Word8,
		   xfl :: Word8,
		   os :: Word8 
		 }
$(devArbitrary ''MGzipFile)

mencode :: MGzipFile -> L.ByteString
mencode (p,bs) = compressWith p bs
