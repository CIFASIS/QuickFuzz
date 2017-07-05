{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.QuickFuzz.Gen.Code.C where

import Data.Default

import Language.C 
import Language.C.Data.Position
import Language.C.Data.Ident

import Test.QuickCheck
import Control.Monad
import Control.DeepSeq
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.List

import Test.QuickFuzz.Derive.Arbitrary
import Test.QuickFuzz.Derive.Fixable
import Test.QuickFuzz.Derive.NFData
import Test.QuickFuzz.Derive.Show
import Test.QuickFuzz.Derive.Mutation

import Test.QuickFuzz.Gen.FormatInfo
import Test.QuickFuzz.Gen.Base.ByteString
import Test.QuickFuzz.Gen.Base.String

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString as BS

instance NFData Position where
  rnf x = ()

--data CCode = CCode (CTranslationUnit NodeInfo)
--unCCode (CCode x) = x

devArbitrary ''CTranslUnit
devNFData ''CTranslationUnit
devMutation ''CTranslUnit

decodeC x = either undefined id $ parseC (L8.toStrict x) (initPos "")

cInfo :: FormatInfo CTranslUnit NoActions
cInfo = def 
    { -- encode = L8.pack . concat . (map show) . (map pretty)
      encode = L8.pack . show . pretty
    , decode = decodeC
    , mutate = mutt
    , random = arbitrary
    , value = show
    , ext = "c" 
    } 
