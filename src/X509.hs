{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module X509 where

import Test.QuickCheck
import DeriveArbitrary
import Data.X509
import Data.ASN1.Types
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding

import qualified Data.ByteString.Lazy as L

$(devArbitrary ''Certificate)

mencode :: Certificate -> L.ByteString
mencode x = L.fromStrict $ encodeASN1' DER (toASN1 x [])
