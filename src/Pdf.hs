{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances #-}

module Pdf where

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

type DrawUnit = Draw ()



$(devActions ["Graphics.EasyRender"] ''Draw True [''()])
$(devArbitrary ''DrawAction)
-- $(devArbitraryWithActions True ''Draw)
$(devShow ''DrawAction)

instance Arbitrary (Draw ()) where
    arbitrary = do
        x <- arbitrary :: Gen [DrawAction]
        return $ performDraw x 


type DocumentUnit = Document ()
$(devActions ["Graphics.EasyRender"] '' Document True [''() ])
-- $(devArbitrary ''DocumentAction)
-- $(devArbitraryWithActions True ''DocumentUnit)
-- $(devShow ''DocumentUnitAction)


instance Show (Document ()) where
    show x = "<doc>"
 
instance Arbitrary (Document ()) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        d <- arbitrary  
        return $ newpage x y d 

instance Arbitrary String where
   arbitrary = mgenName

mencode :: Document () -> L8.ByteString
mencode xs = L8.pack $ render_string Format_PDF  xs
