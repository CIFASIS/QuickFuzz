{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.QuickFuzz.Gen.Code.Python where

import Data.Default

import Test.QuickCheck

import Data.ByteString.Lazy.Char8

import Language.Python.Common
import Language.Python.Common.Pretty

import Test.QuickFuzz.Derive.Arbitrary
import Test.QuickFuzz.Derive.Fixable
import Test.QuickFuzz.Derive.Show
import Test.QuickFuzz.Gen.FormatInfo
import Test.QuickFuzz.Gen.Base.ByteString
import Test.QuickFuzz.Gen.Base.String


type Py = Module ()

devArbitrary ''Py
-- devShow ''	

pyInfo :: FormatInfo Py NoActions
pyInfo = def 
    { encode = pack . prettyText
    , random = arbitrary
    , value = show
    , ext = "py" 
    } 
