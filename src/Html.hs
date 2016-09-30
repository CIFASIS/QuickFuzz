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

--data MHtml = MHtml Html

$(devMArbitrary "Text.Blaze.Html5" "Html")

-- $(devShow ''Action_Html)
-- $(devArbitrary ''Action_Html)

--instance Arbitrary String where
--   arbitrary = mgenName

--mencode :: MHtml -> L8.ByteString
--mencode (MHtml x) = L8.pack $ render $ htmlprint x
