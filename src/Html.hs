{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module Html where

import Test.QuickCheck

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as C8

import Data.DeriveTH

import DeriveMArbitrary
import DeriveArbitrary
import DeriveShow

import ByteString
import Strings

import Text.Blaze.Internal
import Text.Blaze.Html
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.String

import Data.Text
import Data.ByteString.Lazy


$(devActions "Text.Blaze.Html5.Attributes" ''Attribute False [])
$(devArbitrary ''AttributeAction)
$(devArbitraryWithActions False ''Attribute)
-- $(devShow ''AttributeAction)


$(devActions "Text.Blaze.Html5" ''Html True [''Html, ''String])
$(devArbitrary ''HtmlAction)
$(devArbitraryWithActions True ''Html)
-- $(devShow ''HtmlAction)


instance Show HtmlAction  where
   show x = "(noshow)"

instance Arbitrary String where
   arbitrary = oneof [mgenName, return "999"]

mencode :: Html -> L8.ByteString
mencode xs = L8.pack $ renderHtml xs 
