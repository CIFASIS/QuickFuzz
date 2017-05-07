{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.QuickFuzz.Gen.Code.Lua where

import Data.Default

import Test.QuickCheck
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.List

import Language.Lua.Syntax
import Language.Lua.PrettyPrinter

import Test.QuickFuzz.Derive.Arbitrary
import Test.QuickFuzz.Derive.Fixable
import Test.QuickFuzz.Derive.Show
import Test.QuickFuzz.Gen.FormatInfo
import Test.QuickFuzz.Gen.Base.ByteString
import Test.QuickFuzz.Gen.Base.String

import qualified Data.ByteString.Lazy.Char8 as L8

devArbitrary ''Block

luaInfo :: FormatInfo Block NoActions
luaInfo = def 
    { encode = L8.pack . show . pprint
    , random = arbitrary
    , value = show
    , ext = "lua" 
    } 
