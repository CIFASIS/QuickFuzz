{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module Svg where

import Test.QuickCheck

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.DeriveTH

import DeriveMArbitrary
import DeriveArbitrary
import DeriveShow

import ByteString
import Strings

import Text.Blaze.Svg.Internal
import Text.Blaze.Internal
import Text.Blaze.Svg11
import Text.Blaze.Svg11.Attributes
import Text.Blaze.Svg.Renderer.String

-- import Data.Text.Internal 
-- import Data.Text.Internal.Lazy   
-- import Data.ByteString

$(devMArbitrary "Text.Blaze.Svg11" ''Svg True [])
$(devArbitrary ''SvgAction)
$(devShow ''SvgAction)

-- $(devMArbitrary "Text.Blaze" ''AttributeValue False [''Int, ''Svg])


instance Arbitrary String where
   arbitrary = mgenName

mencode :: [SvgAction] -> L8.ByteString
mencode xs = L8.pack $ renderSvg $ performSvg xs



