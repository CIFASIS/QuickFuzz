{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module Html where

import Test.QuickCheck

import Text.XML.HaXml.Types
import Text.XML.HaXml.Html.Generate
import Text.PrettyPrint.HughesPJ

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.DeriveTH
import DeriveArbitrary

import Megadeth.DeriveShow

import Vector
import ByteString
import Strings

data MHtml = MHtml [Content ()]

$(devShow ''MHtml)
$(devArbitrary ''MHtml)

instance Arbitrary String where
   arbitrary = mgenName

mencode :: MHtml -> L8.ByteString
mencode (MHtml x) = L8.pack $ render $ htmlprint x
