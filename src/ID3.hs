{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module ID3 where

import Test.QuickCheck
import Data.Binary( Binary(..), encode )

import ID3.Type.Tag
import ID3.Type.Unparse
import ID3.Type.Frame

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

import Data.DeriveTH
import Data.Map 

import DeriveArbitrary
import ByteString
import Vector

$(devArbitrary ''ID3Tag)

type MID3  = ID3Tag

mencode :: MID3 -> L.ByteString
mencode = L.pack . unparse
