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

import Text.Blaze.Svg11
import Text.Blaze.Svg.Renderer.String


$(devMArbitrary "Text.Blaze.Svg11" ''Svg)

$(devArbitrary ''SvgAction)

$(devShow ''SvgAction)

instance Arbitrary String where
   arbitrary = mgenName

mencode :: SvgAction -> L8.ByteString
mencode x = L8.pack $ renderSvg $ performSvg [x]



