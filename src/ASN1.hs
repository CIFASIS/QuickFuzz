{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module ASN1 where

import Test.QuickCheck
import DeriveArbitrary
import Data.ASN1.Stream
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding

import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Lazy as L
--import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString as BS
import Misc

--data MASN1 = MASN1 [ASN1]
-- $(devArbitrary ''MASN1)
$(devArbitrary ''ASN1)

mencode :: [ASN1] -> L.ByteString
mencode x = L.fromStrict $ encodeASN1' DER x
