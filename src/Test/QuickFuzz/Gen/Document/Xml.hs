{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.QuickFuzz.Gen.Document.Xml where

import Data.Default

import Test.QuickCheck

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as C8

import Data.DeriveTH

import Test.QuickFuzz.Derive.Actions
import Test.QuickFuzz.Derive.Arbitrary
import Test.QuickFuzz.Derive.Show
import Test.QuickFuzz.Gen.FormatInfo
import Test.QuickFuzz.Gen.Base.ByteString
import Test.QuickFuzz.Gen.Base.String

import Data.Text hiding (map)
import Data.Monoid
import Blaze.ByteString.Builder
import Text.XML.Generator

type XmlElem = Xml Elem
type XmlDoc = Xml Doc

$(devActions ["Text.XML.Generator"] ''XmlElem False [''XmlElem, ''Text] ['xtextRaw])
$(devArbitrary ''XmlElemAction)
-- $(devArbitraryWithActions False ''XmlElem)
$(devShow ''XmlElemAction)

instance Arbitrary XmlElem where
    arbitrary = do
        elems <- arbitrary
        return $ xelems (map performXmlElemAction elems)


instance Arbitrary XmlDoc where
    arbitrary = do 
        elems <- arbitrary 
        return $ doc defaultDocInfo (xelems elems)

instance (Show XmlDoc) where
    show _ = "<not-implemented>"

xmlInfo :: FormatInfo XmlDoc XmlElemAction
xmlInfo = def 
    { encode = xrender 
    , random = arbitrary
    , actions = Just $ def
        { randomActions = arbitrary
        , shrinkActions = shrinkXmlElemAction
        , performActions = doc defaultDocInfo . performXmlElemAction 
        , valueActions = show }
    , ext = "xml"
    } 

