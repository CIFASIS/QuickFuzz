{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.QuickFuzz.Gen.Code.GLSL where

import Data.Default

import Test.QuickCheck hiding (Discard)
import Control.DeepSeq
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.List

import Data.ByteString.Lazy.Char8 as L8
import Data.ByteString.Lazy as L
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)

import Text.PrettyPrint.HughesPJClass

import Language.GLSL.Syntax
import Language.GLSL.Parser
import Language.GLSL.Pretty

import Test.QuickFuzz.Derive.Arbitrary
import Test.QuickFuzz.Derive.Fixable
import Test.QuickFuzz.Derive.Show
import Test.QuickFuzz.Derive.NFData
import Test.QuickFuzz.Derive.Mutation
import Test.QuickFuzz.Gen.FormatInfo
import Test.QuickFuzz.Gen.Base.ByteString
import Test.QuickFuzz.Gen.Base.String

import qualified Data.ByteString.Lazy.Char8 as L8

devArbitrary ''TranslationUnit
devMutation ''TranslationUnit
devNFData ''TranslationUnit

decodeglsl x = either undefined id $ parse (Data.Text.unpack $ decodeUtf8 $ L.toStrict x)

glslInfo :: FormatInfo TranslationUnit NoActions
glslInfo = def 
    { encode = L8.pack . render . option . Just
    , decode = decodeglsl
    , mutate = mutt
    , random = arbitrary
    , value = show
    , ext = "glsl" 
    } 
