{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module Xml where

import Test.QuickCheck

import Text.XML.Light.Output( ppcTopElement, prettyConfigPP )
import Text.XML.Light.Types

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC8

import Data.DeriveTH
import DeriveArbitrary

import Vector
import ByteString

import Data.Char (chr)

genName :: Gen String
genName = listOf1 validChars :: Gen String
  where validChars = chr <$> choose (97, 122)

instance Arbitrary String where
   --arbitrary = oneof $ map return ["a", "b", "defs"]
   arbitrary = genName

type MXml = Element

derive makeArbitrary ''Element

$(deriveArbitraryRec ''Content)
$(deriveArbitraryRec ''Attr)

mencode :: MXml -> LC8.ByteString
mencode x = LC8.pack $ ppcTopElement prettyConfigPP x
