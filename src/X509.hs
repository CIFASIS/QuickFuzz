{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module X509 where

import Test.QuickCheck
--import Test.QuickCheck.Instances
import DeriveArbitrary
import Data.Certificate.X509

import Time

--import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Lazy as L
--import qualified Data.ByteString as BS

$(devArbitrary ''X509)

mencode :: X509 -> L.ByteString
mencode x = encodeCertificate x
