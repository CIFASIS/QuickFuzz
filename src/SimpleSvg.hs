{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module SimpleSvg where

import Test.QuickCheck

import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Char8 as C8
import Data.DeriveTH

import DeriveArbitrary
import ByteString
import Vector

import Text.XML.Light.Input( parseXMLDoc )
import Text.XML.Light.Output( ppcTopElement, prettyConfigPP )

import Data.Monoid
import Data.List.Split
import Data.Char (chr)
import qualified Data.Text as T

import Linear

import Text.XML.YJSVG


type MSvgFile  = (Double, Double, [(String, SVG)])

genName :: Gen String
genName = listOf1 validChars :: Gen String
  where validChars = chr <$> choose (97, 122)

instance Arbitrary String where
   arbitrary = genName

instance Arbitrary Color where
   arbitrary = do 
     r <- arbitrary
     g <- arbitrary
     b <- arbitrary
     return $ RGB r g b


-- $(derive makeArbitrary ''MSvgFile)
derive makeArbitrary ''SVG
$(deriveArbitraryRec ''Transform)
$(deriveArbitraryRec ''Position)
$(deriveArbitraryRec ''Font)

-- $(derive makeArbitrary ''Transform)

encodeMSvgFile (x,y,d) = LC8.pack $  showSVG x y d

mencode :: MSvgFile -> LC8.ByteString
mencode = encodeMSvgFile
