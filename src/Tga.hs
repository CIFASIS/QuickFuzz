{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Tga where

import Test.QuickCheck
import Data.Binary( Binary(..), encode )

import Codec.Picture.Types
import Codec.Picture.Tga.Types
import Codec.Picture.Metadata.Exif
import Codec.Picture.Metadata

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

import Data.DeriveTH
import DeriveArbitrary
import ByteString

derive makeShow ''TgaFile_t
derive makeShow ''TgaImageType
derive makeShow ''TgaImageDescription
derive makeShow ''TgaColorMapType
derive makeShow ''TgaHeader

derive makeArbitrary ''TgaFile_t
$(deriveArbitraryRec ''TgaHeader)

mencode :: TgaFile_t -> L.ByteString
mencode = encode
