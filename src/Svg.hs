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


$(devMArbitrary "Text.Blaze.Svg11.Attributes" ''Attribute False [])
$(devArbitrary ''AttributeAction)


instance Arbitrary Attribute where
    arbitrary = do 
        x <- arbitrary :: Gen AttributeAction   
        return $ performAttribute x 

$(devMArbitrary "Text.Blaze.Svg11" ''Svg True [''Svg, ''String])
$(devArbitrary ''SvgAction)
-- $(devShow ''SvgAction)


instance Show SvgAction  where
   show x = "(noshow)"

instance Arbitrary String where
   arbitrary = oneof [mgenName, return "999"]

mencode :: [SvgAction] -> L8.ByteString
mencode xs = L8.pack $ renderSvg $ performSvg xs



