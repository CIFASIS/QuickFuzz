{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module Html where

import Test.QuickCheck

import Text.XML.HaXml.Types
import Text.XML.HaXml.Html.Generate
import Text.XML.HaXml.Html.Parse
import Text.XML.HaXml.Posn
import Text.PrettyPrint.HughesPJ

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as C8

import Data.DeriveTH
import DeriveArbitrary
import DeriveMutation
import DeriveShow

import Vector
import ByteString
import Strings

data MHtml = MHtml [Content Posn]

$(devShow ''MHtml)
$(devArbitrary ''MHtml)
$(devMutation ''MHtml)

instance Arbitrary String where
   arbitrary = mgenName

mencode :: MHtml -> L8.ByteString
mencode (MHtml x) = L8.pack $ render $ htmlprint x

elementtoContent :: Document Posn -> [Content Posn]
elementtoContent (Document _ _ (Elem _ _ xs)  _ ) = xs

mdecode :: C8.ByteString -> MHtml
mdecode html =  MHtml $ elementtoContent $ htmlParse "" (C8.unpack html) 

