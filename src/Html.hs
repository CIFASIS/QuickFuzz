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

import Text.Blaze.Html5
import Text.Blaze.Html.Renderer.String


$(devMArbitrary "Text.Blaze.Html5" ''Html)

$(devArbitrary ''HtmlAction)

$(devShow ''HtmlAction)

instance Arbitrary String where
   arbitrary = mgenName

mencode :: HtmlAction -> L8.ByteString
mencode x = L8.pack $ renderHtml $ performHtml x
