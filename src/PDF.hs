{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances #-}

module PDF where

import Test.QuickCheck
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.DeriveTH

import DeriveMArbitrary
import DeriveArbitrary
import DeriveShow

import ByteString
import Strings

import Graphics.EasyRender
import Graphics.EasyRender.Internal


$(devMArbitrary "Graphics.EasyRender" ''Draw)
$(devArbitrary ''DrawAction)
$(devShow ''DrawAction)

-- $(devMArbitrary "Graphics.EasyRender" ''Document)

instance Arbitrary String where
   arbitrary = mgenName


mencode :: DrawAction -> L8.ByteString
mencode x = L8.pack $ render_string Format_PDF $ newpage 10 20 $ performDraw [x]
