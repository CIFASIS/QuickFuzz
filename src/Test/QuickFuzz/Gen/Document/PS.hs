{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.QuickFuzz.Gen.Document.PS where

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

psInfo :: FormatInfo (Draw()) [DrawAction]
psInfo = def 
    { encode = L8.pack . render_string Format_PS . newpage 1024 1024 
    , random = arbitrary
    , actions = Just $ def
        { randomActions = arbitrary
        , shrinkActions = shrinkActionList shrinkDrawAction   
        , performActions = performDrawAction }
    , ext = "ps" 
    } 
