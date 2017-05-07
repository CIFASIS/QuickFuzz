{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.QuickFuzz.Gen.Pki.ASN1 where

import Data.Default

import Data.ASN1.Types
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding

import Test.QuickCheck
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.List
import Data.Monoid 

import Test.QuickFuzz.Derive.Arbitrary

import Test.QuickFuzz.Derive.Show
import Test.QuickFuzz.Gen.FormatInfo
import Test.QuickFuzz.Gen.Base.ByteString
import Test.QuickFuzz.Gen.Base.String

import qualified Data.ByteString.Lazy as L

devArbitrary ''ASN1

asn1Info :: FormatInfo [ASN1] NoActions
asn1Info = def 
    { encode = L.fromStrict . (encodeASN1' DER)
    , random = arbitrary
    , value = show
    , ext = "asn1" 
    } 
