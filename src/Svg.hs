{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module Svg where

import Test.QuickCheck

--import Data.Binary( Binary(..), encode )
import Graphics.Svg.Types

import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Char8 as C8
import Data.DeriveTH

import DeriveArbitrary
import ByteString
import Vector

import Text.XML.Light.Input( parseXMLDoc )
import Text.XML.Light.Output( ppcTopElement, prettyConfigPP )

import Graphics.Svg.XmlParser
import Graphics.Svg.CssTypes
import Graphics.Svg.Types
import Data.Map.Strict
import qualified Data.Text.Internal as DT

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS

import Data.Text.Array

import Data.Monoid
--import Data.List.Split
import Data.Char (chr)
import qualified Data.Text as T

import Linear

type MSvgFile  = Document

instance  Arbitrary DT.Text where
   arbitrary = do 
     --a1 <- arbitrary 
     oneof $ Prelude.map (return . T.pack) ["a", "b", "c", "d", "e"]--genName
 -- $ a1
  

instance Arbitrary String where
  --arbitrary = genName
  arbitrary = oneof $ Prelude.map return ["a", "b", "c", "d", "e"]--genName

instance Arbitrary a => Arbitrary (Last a) where
    arbitrary = do
        ga <- arbitrary
        oneof $ Prelude.map (return . Last) [Nothing, Just ga]


instance Arbitrary RPoint where
   arbitrary = do 
     a1 <- arbitrary
     a2 <- arbitrary
     return $ V2 a1 a2

instance Arbitrary Data.Text.Array.Array where
      arbitrary
        = undefined

instance Arbitrary ((Coord, Coord, Coord, Bool, Bool, RPoint)) where
   arbitrary = (,,,,,)  <*>arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
 --do 
     -- a1 <- arbitrary
     -- a2 <- arbitrary
     -- a3 <- arbitrary
     -- a4 <- arbitrary
     -- a5 <- arbitrary
     -- a6 <- arbitrary
     -- return $ (a1, a2, a3, a4, a5, a6)



-- derive makeArbitrary ''Symbol
-- derive makeArbitrary ''Group

-- $(showDeps ''Use)
-- $(showDeps ''Symbol)
$(showDeps ''MSvgFile)

genName :: Gen String
genName = listOf1 validChars :: Gen String
  where validChars = chr <$> choose (97, 122)
   
  
encodeMSvgFile = LC8.pack . ppcTopElement prettyConfigPP . xmlOfDocument
   
mencode :: MSvgFile -> LC8.ByteString
mencode = encodeMSvgFile
