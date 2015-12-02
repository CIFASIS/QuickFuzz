{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Zip where

import DeriveArbitrary
import Data.Binary( Binary(..), encode )
import Codec.Archive.Zip

import qualified Data.ByteString.Lazy as L
import Vector
import ByteString

$(deriveArbitraryRec ''Archive)

mencode :: Archive -> L.ByteString
mencode = encode
