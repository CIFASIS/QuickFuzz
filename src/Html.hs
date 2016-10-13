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


-- $(devMArbitrary False "Text.Blaze.Html5" ''AttributeValue)

-- $(devMArbitrary False [] "Text.Blaze.Html5.Attributes" ''Attribute)
-- $(devArbitrary ''AttributeAction)
-- $(devShow ''AttributeAction)

-- $(devMArbitrary True [] "Text.Blaze.Html5" ''Html)
-- $(devArbitrary ''HtmlAction)
-- $(devShow ''HtmlAction)


--instance Show AttributeAction  where
--   show x = "(noshow)"

instance Arbitrary String where
   arbitrary = mgenName

--mencode :: [([HtmlAction], AttributeAction)] -> L8.ByteString
--mencode xs = L8.pack $ renderHtml $ Prelude.foldr1 (>>) $ Prelude.map h xs 
--    where h (html, attr) = performHtml html ! performAttribute attr
