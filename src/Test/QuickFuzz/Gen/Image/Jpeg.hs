{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.QuickFuzz.Gen.Image.Jpeg where

import Data.Default

import qualified Data.Binary

import Codec.Picture.Jpg
import Codec.Picture.Jpg.Types
import Codec.Picture.Tiff.Types
import Codec.Picture.Jpg.DefaultTable

import Test.QuickCheck
import Control.Monad
import Control.DeepSeq
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.List
import Data.Monoid 
import Data.Word(Word8, Word16, Word32)

import Test.QuickFuzz.Derive.Arbitrary
import Test.QuickFuzz.Derive.Mutation
import Test.QuickFuzz.Derive.Actions
import Test.QuickFuzz.Derive.Show
import Test.QuickFuzz.Derive.NFData

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
devMutation ''JpgImage
devShow ''JpgImage
devNFData ''JpgImage

jpegencode :: JpgImage -> L.ByteString
jpegencode = Data.Binary.encode

jpegdecode :: L.ByteString -> JpgImage
jpegdecode = Data.Binary.decode

jpegInfo :: FormatInfo JpgImage NoActions
jpegInfo = def 
    { encode = jpegencode
    , decode = jpegdecode
    , mutate = mutt
    , random = arbitrary
    , value = show
    , ext = "jpg" 
    } 
