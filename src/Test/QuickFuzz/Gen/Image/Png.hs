{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.QuickFuzz.Gen.Image.Png where

import Data.Default

import qualified Data.Binary

import Codec.Picture.Png
import Codec.Picture.Png.Type
import Codec.Picture.Png.Export
import Codec.Picture.Metadata
import Codec.Picture.ColorQuant

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
import Test.QuickFuzz.Gen.Base.Image

import qualified Data.ByteString.Lazy as L

devArbitrary ''PngRawImage
devShow ''PngRawImage

pngencode :: PngRawImage -> L.ByteString
pngencode = Data.Binary.encode

pngInfo :: FormatInfo PngRawImage NoActions
pngInfo = def 
    { encode = pngencode
    , random = arbitrary
    , value = show
    , ext = "png" 
    } 
