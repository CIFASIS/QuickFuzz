{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.QuickFuzz.Gen.Image.Tga where

import Data.Default

import qualified Data.Binary

import Test.QuickCheck
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.List
--import Data.Monoid 
--import Data.Word(Word8, Word16, Word32)

import Codec.Picture.Types
import Codec.Picture.Tga.Types
import Codec.Picture.Metadata.Exif
import Codec.Picture.Metadata

import Test.QuickFuzz.Derive.Arbitrary
import Test.QuickFuzz.Derive.Actions
import Test.QuickFuzz.Derive.Show
import Test.QuickFuzz.Gen.FormatInfo
import Test.QuickFuzz.Gen.Base.ByteString
import Test.QuickFuzz.Gen.Base.String
import Test.QuickFuzz.Gen.Base.Image
import Test.QuickFuzz.Gen.Base.Vector

import qualified Data.ByteString.Lazy as L

devArbitrary ''TgaFile_t
devShow ''TgaFile_t

tgaencode :: TgaFile_t -> L.ByteString
tgaencode = Data.Binary.encode

tgaInfo :: FormatInfo TgaFile_t NoActions
tgaInfo = def 
    { encode = tgaencode
    , random = arbitrary
    , value = show
    , ext = "tga" 
    } 
