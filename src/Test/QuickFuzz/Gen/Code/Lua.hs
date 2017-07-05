{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.QuickFuzz.Gen.Code.Lua (luaInfo) where

import Data.Default

import Test.QuickCheck
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.List

import Language.Lua.Syntax
import Language.Lua.PrettyPrinter
import Language.Lua.Parser

import Test.QuickFuzz.Derive.Arbitrary
import Test.QuickFuzz.Derive.Fixable
import Test.QuickFuzz.Derive.Show
import Test.QuickFuzz.Derive.Mutation
import Test.QuickFuzz.Gen.FormatInfo
import Test.QuickFuzz.Gen.Base.ByteString
import Test.QuickFuzz.Gen.Base.String

import qualified Data.ByteString.Lazy.Char8 as L8
import Data.ByteString.Lazy as L
import Data.Text.Encoding (decodeUtf8)

devArbitrary ''Block
devMutation ''Block

decode' x = either undefined id $ parseText chunk (decodeUtf8 $ L.toStrict x) 

luaInfo :: FormatInfo Block NoActions
luaInfo = def 
    { encode = L8.pack . show . pprint
    , decode = decode'
    , mutate = mutt
    , random = arbitrary
    , value = show
    , ext = "lua" 
    } 
