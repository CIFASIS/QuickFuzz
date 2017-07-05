{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.QuickFuzz.Gen.Code.Js where

import Data.Default

import Test.QuickCheck
import Control.DeepSeq
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.List
--import Text.Parsec.Pos

import Data.ByteString.Lazy.Char8 as L8
import Data.ByteString.Lazy as L
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)

import Language.ECMAScript3.PrettyPrint
import Language.ECMAScript3.Syntax
import Language.ECMAScript3.Parser

import Test.QuickFuzz.Derive.Arbitrary
import Test.QuickFuzz.Derive.Fixable
import Test.QuickFuzz.Derive.Show
import Test.QuickFuzz.Derive.NFData
import Test.QuickFuzz.Derive.Mutation
import Test.QuickFuzz.Gen.FormatInfo
import Test.QuickFuzz.Gen.Base.ByteString
import Test.QuickFuzz.Gen.Base.String

import qualified Data.ByteString.Lazy.Char8 as L8

type Js = JavaScript SourcePos
devArbitrary ''Js
devMutation ''Js
devNFData ''JavaScript

instance NFData SourcePos where
  rnf x = ()

--devNFData ''SourcePos

decode' x = either undefined id $ parseFromString (Data.Text.unpack $ decodeUtf8 $ L.toStrict x)

jsInfo :: FormatInfo Js NoActions
jsInfo = def 
    { encode = L8.pack . show . prettyPrint
    , decode = decode'
    , mutate = mutt
    , random = arbitrary
    , value = show
    , ext = "js" 
    } 
