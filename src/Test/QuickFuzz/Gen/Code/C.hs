{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.QuickFuzz.Gen.Code.C where

import Data.Default

import Language.C 

import Test.QuickCheck
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.List

import Test.QuickFuzz.Derive.Arbitrary
import Test.QuickFuzz.Derive.Fixable

import Test.QuickFuzz.Derive.Show
import Test.QuickFuzz.Gen.FormatInfo
import Test.QuickFuzz.Gen.Base.ByteString
import Test.QuickFuzz.Gen.Base.String

import qualified Data.ByteString.Lazy.Char8 as L8

devArbitrary ''CTranslUnit
-- devShow ''	

cInfo :: FormatInfo [CTranslUnit] NoActions
cInfo = def 
    { encode = L8.pack . concat . (map show) . (map pretty)
    , random = arbitrary
    , value = show
    , ext = "c" 
    } 
