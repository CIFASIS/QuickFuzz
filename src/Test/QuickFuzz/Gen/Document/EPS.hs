{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.QuickFuzz.Gen.Document.EPS where

import Data.Default
import Test.QuickCheck

import qualified Data.ByteString.Lazy.Char8 as L8

import Graphics.EasyRender
import Graphics.EasyRender.Internal

import Test.QuickFuzz.Derive.Actions
import Test.QuickFuzz.Derive.Arbitrary
import Test.QuickFuzz.Derive.Show
import Test.QuickFuzz.Gen.FormatInfo
import Test.QuickFuzz.Gen.Base.ByteString
import Test.QuickFuzz.Gen.Base.String

import Test.QuickFuzz.Gen.Document.PDF

epsInfo :: FormatInfo (Draw()) [DrawAction]
epsInfo = def 
    { encode = L8.pack . render_string (Format_EPS 0) . newpage 1024 1024 
    , random = arbitrary
    , actions = Just $ def
        { randomActions = arbitrary
        , shrinkActions = shrinkActionList shrinkDrawAction   
        , performActions = performDrawAction }
    , ext = "eps" 
    } 
