{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.QuickFuzz.Gen.Network.HTTP where

import Data.Default
import Data.Text.Encoding (encodeUtf8)

import Network.HTTP.Headers
import Network.HTTP.Base

import Test.QuickCheck

import Test.QuickFuzz.Derive.Arbitrary
import Test.QuickFuzz.Derive.Show
import Test.QuickFuzz.Gen.FormatInfo
import Test.QuickFuzz.Gen.Base.ByteString
import qualified Data.ByteString.Lazy.Char8 as L8


devArbitrary ''Request
devShow ''Request


httpRequestInfo :: FormatInfo (Request String) NoActions
httpRequestInfo = def 
    { encode = L8.pack . show
    , random = arbitrary
    , value = show 
    , ext = "http"
    }

devArbitrary ''Response
devShow ''Response

httpResponseInfo :: FormatInfo (Response String) NoActions
httpResponseInfo = def
    { encode = L8.pack . show
    , random = arbitrary
    , value = show 
    , ext = "http"
    } 
