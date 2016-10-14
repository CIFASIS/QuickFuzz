{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module Html where

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
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.String

--import Data.Text
--import Data.ByteString.Lazy

-- $(devMArbitrary "Text.Blaze.Internal" ''AttributeValue False [])

$(devMArbitrary "Text.Blaze.Html5.Attributes" ''Attribute False [])
$(devArbitrary ''AttributeAction)
-- $(devShow ''AttributeAction)

   
$(devMArbitrary "Text.Blaze.Html5" ''Html True [''Html, ''String])
$(devArbitrary ''HtmlAction)
$(devShow ''HtmlAction)


instance Show AttributeAction  where
   show x = "(noshow)"

instance Arbitrary String where
   arbitrary = mgenName

mencode :: [([HtmlAction], AttributeAction)] -> L8.ByteString
mencode xs = L8.pack $ renderHtml $ Prelude.foldr1 (>>) $ Prelude.map h xs 
    where h (html, attr) = performHtml html ! performAttribute attr
