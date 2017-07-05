{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.QuickFuzz.Gen.Code.Python (pyInfo) where

import Data.Default

import Test.QuickCheck

import Data.ByteString.Lazy.Char8 as L8
import Data.ByteString.Lazy as L
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)

import Control.DeepSeq
import Language.Python.Common
import Language.Python.Common.Pretty
import Language.Python.Version2.Parser

import Test.QuickFuzz.Derive.Arbitrary
import Test.QuickFuzz.Derive.Fixable
import Test.QuickFuzz.Derive.Show
import Test.QuickFuzz.Derive.NFData
import Test.QuickFuzz.Derive.Mutation

import Test.QuickFuzz.Gen.FormatInfo
import Test.QuickFuzz.Gen.Base.ByteString
import Test.QuickFuzz.Gen.Base.String


type Py = Module SrcSpan

devArbitrary ''Py
devNFData ''Module
devNFData ''SrcSpan
devMutation ''Py

decode' x = either undefined fst $ parseModule (Data.Text.unpack $ decodeUtf8 $ L.toStrict x) ""

pyInfo :: FormatInfo Py NoActions
pyInfo = def 
    { encode = L8.pack . prettyText
    , decode = decode' 
    , mutate = mutt
    , random = arbitrary
    , value = show
    , ext = "py" 
    } 
