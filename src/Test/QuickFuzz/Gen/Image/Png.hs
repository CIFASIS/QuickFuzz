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
import Control.DeepSeq
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.List
import Data.Monoid 

import Test.QuickFuzz.Derive.Arbitrary
import Test.QuickFuzz.Derive.Mutation
import Test.QuickFuzz.Gen.FormatInfo
import Test.QuickFuzz.Derive.Show
import Test.QuickFuzz.Derive.NFData
import Test.QuickFuzz.Gen.FormatInfo
import Test.QuickFuzz.Gen.Base.ByteString
import Test.QuickFuzz.Gen.Base.String
import Test.QuickFuzz.Gen.Base.Image

import qualified Data.ByteString.Lazy as L

devArbitrary ''PngRawImage
devMutation ''PngRawImage
devShow ''PngRawImage
devNFData ''PngRawImage

pngencode :: PngRawImage -> L.ByteString
pngencode = Data.Binary.encode

pngdecode :: L.ByteString -> PngRawImage 
pngdecode = Data.Binary.decode

pngInfo :: FormatInfo PngRawImage NoActions
pngInfo = def 
    { encode = pngencode
    , decode = pngdecode
    , random = arbitrary
    , mutate = mutt
    , value = show
    , ext = "png" 
    } 
