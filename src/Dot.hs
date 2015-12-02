{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Dot where

import Test.QuickCheck
import Data.Binary( Binary(..), encode )
import Data.DeriveTH

import DeriveArbitrary
import ByteString
import Vector
import Images

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

import Language.Dot.Syntax
import Language.Dot.Pretty

import Data.Char (chr)
--import Data.List.Split

genName :: Gen String
genName = listOf1 validChars :: Gen String
  where validChars = chr <$> choose (97, 122)

instance Arbitrary String where
   arbitrary = genName

derive makeArbitrary ''Graph
derive makeArbitrary ''Statement
derive makeArbitrary ''Subgraph
derive makeArbitrary ''Id
derive makeArbitrary ''NodeId
derive makeArbitrary ''GraphDirectedness
derive makeArbitrary ''AttributeStatementType
derive makeArbitrary ''Xml
derive makeArbitrary ''Entity
derive makeArbitrary ''GraphStrictness
derive makeArbitrary ''XmlAttribute

derive makeArbitrary ''XmlAttributeValue
derive makeArbitrary ''XmlName
derive makeArbitrary ''EdgeType
derive makeArbitrary ''Attribute
derive makeArbitrary ''Port
derive makeArbitrary ''Compass

--instance Arbitrary Id where
--    arbitrary = do
--      s <- (arbitrary :: Gen String)
--      i <- (arbitrary :: Gen Integer)
--      x <- (arbitrary :: Gen Xml)
--      oneof $ map return ([NameId s, StringId s, IntegerId i, FloatId 1.0, XmlId x])

-- $(deriveArbitraryRec ''Graph)

mencode :: Graph -> L8.ByteString
mencode = L8.pack . renderDot 
