{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module CRL where

import Test.QuickCheck
--import Test.QuickCheck.Instances
import DeriveArbitrary
import Data.X509
import Data.ASN1.Types
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding

--import Time

--import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as BS

$(devArbitrary ''CRL)

mencode :: CRL -> L.ByteString
mencode x = L.fromStrict $ encodeASN1' DER (toASN1 x [])
