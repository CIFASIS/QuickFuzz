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

import Data.Monoid
--import Data.List.Split
import Data.Char (chr)
import qualified Data.Text as T

import Linear

type MSvgFile  = Document

instance  Arbitrary DT.Text where
   arbitrary = do 
     a1 <- arbitrary 
     return $ a1

instance  (Arbitrary a) => Arbitrary (Last a) where
   arbitrary = do 
     a1 <- arbitrary 
     return $ return a1


instance Arbitrary ((Coord, Coord, Coord, Bool, Bool, RPoint)) where
   arbitrary = do 
     a1 <- arbitrary
     a2 <- arbitrary
     a3 <- arbitrary
     a4 <- arbitrary
     a5 <- arbitrary
     a6 <- arbitrary
     return $ (a1, a2, a3, a4, a5, a6)


instance Arbitrary RPoint where
   arbitrary = do 
     a1 <- arbitrary
     a2 <- arbitrary
     return $ V2 a1 a2

instance Arbitrary (Map String Element) where
   arbitrary = do
     x <- arbitrary
     y <- arbitrary 
     return $ singleton x y

genName :: Gen String
genName = listOf1 validChars :: Gen String
  where validChars = chr <$> choose (97, 122)

instance Arbitrary String where
   arbitrary = genName
   --arbitrary = oneof $ Prelude.map return ["a", "b", "c", "d", "e"]--genName


-- $(derive makeArbitrary ''Graphics.Svg.Types.Tree)

instance Arbitrary Tree where
  arbitrary = do
               a1 <- arbitrary
               a2 <- arbitrary
               a3 <- arbitrary
               a4 <- arbitrary
               a5 <- arbitrary
               a6 <- arbitrary
               a7 <- arbitrary
               --a8 <- arbitrary
               --a9 <- arbitrary
               a10 <- arbitrary
               --oneof $ Prelude.map return [PathTree a1, CircleTree a2, PolyLineTree a3, PolygonTree a4, EllipseTree a5, LineTree a6, RectangleTree a7, TextTree a8 a9, ImageTree a10] 
               oneof $ Prelude.map return [PathTree a1, CircleTree a2, PolyLineTree a3, PolygonTree a4, EllipseTree a5, LineTree a6, RectangleTree a7,  ImageTree a10] 


$(derive makeArbitrary ''Origin)

--derive makeArbitrary ''Document

instance Arbitrary Document where
   arbitrary = do
     a1 <- arbitrary
     a2 <- arbitrary
     a3 <- arbitrary
     a4 <- arbitrary
     a5 <- arbitrary
     a6 <- arbitrary
     a7 <- arbitrary

     return $ Document { _viewBox = a1
                       , _width = a2
                       , _height =  a3
                       , _elements = a4
                       ,_definitions = a7
                       , _description = a5
                       ,  _styleRules = [], _documentLocation = a6 }

derive makeArbitrary ''ElementRef
derive makeArbitrary ''Texture
-- derive makeArbitrary ''DrawAttributes

instance Arbitrary DrawAttributes where
  arbitrary = do 
    x1 <- arbitrary
    x2 <- arbitrary
    return $ mempty { _transform = x1, _strokeColor = x2 }

derive makeArbitrary ''TextSpan
derive makeArbitrary ''CssElement
derive makeArbitrary ''Element
derive makeArbitrary ''CssDeclaration
derive makeArbitrary ''CssSelector
derive makeArbitrary ''CssDescriptor
derive makeArbitrary ''Mask
derive makeArbitrary ''Marker
derive makeArbitrary ''MarkerUnit
derive makeArbitrary ''MarkerOrientation
derive makeArbitrary ''Text
derive makeArbitrary ''TextAdjust

derive makeArbitrary ''TextSpanContent
derive makeArbitrary ''Pattern
derive makeArbitrary ''TextPath
derive makeArbitrary ''PathCommand
derive makeArbitrary ''Symbol
derive makeArbitrary ''Rectangle
derive makeArbitrary ''Polygon
derive makeArbitrary ''PolyLine
-- $(derive makeArbitrary ''DT.Text)

$(deriveArbitraryRec ''TextInfo)
$(deriveArbitraryRec ''CssRule)
$(deriveArbitraryRec ''RadialGradient)
$(deriveArbitraryRec ''LinearGradient)
$(deriveArbitraryRec ''ClipPath)
$(deriveArbitraryRec ''Use)
$(deriveArbitraryRec ''TextAnchor)
$(deriveArbitraryRec ''TextPathSpacing)
$(deriveArbitraryRec ''TextPathMethod)
$(deriveArbitraryRec ''Path)
$(deriveArbitraryRec ''Line)
$(deriveArbitraryRec ''LineJoin) 
$(deriveArbitraryRec ''Image)
$(deriveArbitraryRec ''Group)
$(deriveArbitraryRec ''Ellipse)
$(deriveArbitraryRec ''Circle)
$(deriveArbitraryRec ''FontStyle)
$(deriveArbitraryRec ''FillRule)
$(deriveArbitraryRec ''Cap)

encodeMSvgFile = LC8.pack . ppcTopElement prettyConfigPP . xmlOfDocument

mencode :: MSvgFile -> LC8.ByteString
mencode = encodeMSvgFile
