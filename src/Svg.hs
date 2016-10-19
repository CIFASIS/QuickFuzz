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

import Text.Blaze.Internal
import Text.Blaze.Svg
import Text.Blaze.Svg.Internal
import Text.Blaze.Svg11
import Text.Blaze.Svg11.Attributes
import Text.Blaze.Svg.Renderer.String


$(devActions "Text.Blaze.Svg" ''Path True [''Int])
$(devArbitrary ''PathAction)
$(devArbitraryWithActions True ''Path)
-- $(devShow ''PathAction)


$(devActions "Text.Blaze.Svg" ''AttributeValue False [''Int, ''Path])
-- $(devArbitrary ''AttributeValueAction)  -- This fails miserably :(
$(devArbitraryWithActions False ''AttributeValue)
-- $(devShow ''AttributeValue)

instance Arbitrary AttributeValueAction where
    arbitrary = do
        i1 <- arbitrary
        i2 <- arbitrary
        i3 <- arbitrary
        i4 <- arbitrary
        i5 <- arbitrary
        i6 <- arbitrary
        p <- arbitrary
        elements 
            [ Act_AttributeValue_matrix_1 i1 i2 i3 i4 i5 i6
            , Act_AttributeValue_mkPath p
            , Act_AttributeValue_rotate_1 i1
            , Act_AttributeValue_rotateAround_1 i1 i2 i3
            , Act_AttributeValue_scale_1 i1 i2
            , Act_AttributeValue_skewX_1 i1
            , Act_AttributeValue_skewY_1 i1 
            , Act_AttributeValue_translate_1 i1 i2 ]


$(devActions "Text.Blaze.Svg11.Attributes" ''Attribute False [''AttributeValue])
$(devArbitrary ''AttributeAction)
$(devArbitraryWithActions False ''Attribute)
-- $(devShow ''Attribute)


$(devActions "Text.Blaze.Svg11" ''Svg True [''Svg, ''Attribute, ''String])
$(devArbitrary ''SvgAction)
$(devArbitraryWithActions True ''Svg)
-- $(devShow ''SvgAction)


instance Show Svg  where
   show x = "(noshow)"

instance Arbitrary String where
   arbitrary = oneof [mgenName, return "999"]

mencode :: Svg -> L8.ByteString
mencode xs = L8.pack $ renderSvg xs
