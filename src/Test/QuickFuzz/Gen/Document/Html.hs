{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Test.QuickFuzz.Gen.Document.Html where

import Data.Default

import Test.QuickCheck hiding (shrink)

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as C8

import Control.DeepSeq
import Data.DeriveTH

import Test.QuickFuzz.Derive.Actions
import Test.QuickFuzz.Derive.Arbitrary
import Test.QuickFuzz.Derive.Show
import Test.QuickFuzz.Gen.FormatInfo hiding (value)
import Test.QuickFuzz.Gen.Base.ByteString
import Test.QuickFuzz.Gen.Base.String

import Text.Blaze.Internal
import Text.Blaze.Html 
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.String


$(devActions ["Text.Blaze.Html5.Attributes"] ''Attribute False [] [])
$(devArbitrary ''AttributeAction)
$(devArbitraryWithActions False ''Attribute)
-- $(devShow ''AttributeAction)

instance (Show Attribute) where
    show _ = "<not-implemented>"

$(devActions ["Text.Blaze.Html5"] ''Html True [''Html, ''String] [])
$(devArbitrary ''HtmlAction)
$(devArbitraryWithActions True ''Html)

$(derive makeShow ''HtmlAction)
-- $(devShow ''HtmlAction)

instance Show Html where
    show _ = "<not-implemented>"

instance NFData Html where
    rnf _ = ()

htmlInfo :: FormatInfo Html [HtmlAction]
htmlInfo = def
    { encode = L8.pack . renderHtml
    , random = arbitrary
    , actions = Just $ def 
        { randomActions = arbitrary
        , shrinkActions = shrinkActionList shrinkHtmlAction
        , performActions = performHtmlAction }
    , ext = "html"
    } 
