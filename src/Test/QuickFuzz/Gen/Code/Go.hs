{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.QuickFuzz.Gen.Code.Go where

import Data.Default

import Test.QuickCheck
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.List

import Language.Go.Syntax.AST
import Language.Go.Pretty  
import Text.PrettyPrint (render)

import Test.QuickFuzz.Derive.Arbitrary
import Test.QuickFuzz.Derive.Fixable
import Test.QuickFuzz.Derive.Show
import Test.QuickFuzz.Gen.FormatInfo
import Test.QuickFuzz.Gen.Base.ByteString
import Test.QuickFuzz.Gen.Base.String

import qualified Data.ByteString.Lazy.Char8 as L8

devArbitrary ''GoSource

goInfo :: FormatInfo GoSource NoActions
goInfo = def 
    { encode = L8.pack . render . pretty
    , random = arbitrary
    , value = show
    , ext = "go" 
    } 
