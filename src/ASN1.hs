{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module ASN1 where

import Test.QuickCheck
import DeriveArbitrary
import Data.ASN1.Types
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding

import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as BS
import Misc

$(devArbitrary ''ASN1)

mencode :: [ASN1] -> L.ByteString
mencode x = L.fromStrict $ encodeASN1' DER x
