{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.QuickFuzz.Gen.Code.Js where

import Data.Default

import Test.QuickCheck
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.List

import Language.ECMAScript3.PrettyPrint
import Language.ECMAScript3.Syntax

import Test.QuickFuzz.Derive.Arbitrary
import Test.QuickFuzz.Derive.Fixable
import Test.QuickFuzz.Derive.Show
import Test.QuickFuzz.Gen.FormatInfo
import Test.QuickFuzz.Gen.Base.ByteString
import Test.QuickFuzz.Gen.Base.String

import qualified Data.ByteString.Lazy.Char8 as L8

type Js = JavaScript ()
devArbitrary ''Js
-- devShow ''	

jsInfo :: FormatInfo Js NoActions
jsInfo = def 
    { encode = L8.pack . show . prettyPrint
    , random = arbitrary
    , value = show
    , ext = "js" 
    } 
