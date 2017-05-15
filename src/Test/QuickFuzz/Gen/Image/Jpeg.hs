{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.QuickFuzz.Gen.Image.Jpeg where

import Data.Default

import qualified Data.Binary

import Codec.Picture.Jpg
import Codec.Picture.Jpg.Types
import Codec.Picture.Jpg.DefaultTable

--import Codec.Picture.Png
--import Codec.Picture.Png.Type
--import Codec.Picture.Png.Export
--import Codec.Picture.Metadata
--import Codec.Picture.ColorQuant

import Test.QuickCheck
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.List
import Data.Monoid 
import Data.Word(Word8, Word16, Word32)

import Test.QuickFuzz.Derive.Arbitrary
import Test.QuickFuzz.Derive.Actions
import Test.QuickFuzz.Derive.Show
import Test.QuickFuzz.Gen.FormatInfo
import Test.QuickFuzz.Gen.Base.ByteString
import Test.QuickFuzz.Gen.Base.String
import Test.QuickFuzz.Gen.Base.Image
import Test.QuickFuzz.Gen.Base.Vector

import qualified Data.ByteString.Lazy as L

$(devActions ["Codec.Picture.Jpg.DefaultTable"] ''HuffmanTree False [] [])
$(devArbitrary ''HuffmanTreeAction)
$(devArbitraryWithActions False ''HuffmanTree)

$(devActions ["Codec.Picture.Jpg.DefaultTable"] ''HuffmanPackedTree False [] [])
$(devArbitrary ''HuffmanPackedTreeAction)
$(devArbitraryWithActions False ''HuffmanPackedTree)

$(devActions ["Codec.Picture.Jpg.DefaultTable"] ''HuffmanTable False [] [])
$(devArbitrary ''HuffmanTableAction)
$(devArbitraryWithActions False ''HuffmanTable)

devArbitrary ''JpgImage
devShow ''JpgImage

jpegencode :: JpgImage -> L.ByteString
jpegencode = Data.Binary.encode

jpegInfo :: FormatInfo JpgImage NoActions
jpegInfo = def 
    { encode = jpegencode
    , random = arbitrary
    , value = show
    , ext = "jpg" 
    } 
