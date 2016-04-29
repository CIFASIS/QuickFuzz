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

import Vector
import ByteString
import Strings

data MHtml = MHtml [Content ()] deriving Show

$(devArbitrary ''MHtml)
$(devIntGen ''MHtml)


instance Arbitrary String where
   arbitrary = mgenName

mgen :: [Int] -> Gen MHtml 
mgen = customGen_Html_MHtml

mencode :: MHtml -> L8.ByteString
mencode (MHtml x) = L8.pack $ render $ htmlprint x
