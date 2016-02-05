{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module Xml where

import Test.QuickCheck
import Text.XML.HaXml.Types
import Text.XML.HaXml.ByteStringPP
--import Text.XML.Light.Output( ppcTopElement, prettyConfigPP )
-- import Text.XML.Light.Types

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

sgenName :: Int -> Gen String
sgenName 1 = do
        c <- chr <$> choose (97,122)
        return $ [c]
sgenName n = do
        c <- chr <$> choose (97,122)
        n <- sgenName (n-1)
        return $ c : n

instance Arbitrary String where
   arbitrary = oneof $ map return ["a", "b", "c"]
   --arbitrary = sized sgenName 

type MXml = Prolog

$(showDeps ''MXml)

mencode :: MXml -> LC8.ByteString
mencode x = prolog x  --LC8.pack $ ppcTopElement prettyConfigPP x

