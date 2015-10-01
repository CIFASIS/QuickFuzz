{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module SimpleSvg where

import Test.QuickCheck
import Check

import Data.Binary( Binary(..), encode )

--import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Char8 as C8
import Data.DeriveTH

import DeriveArbitrary
import ByteString
import Vector

import Text.XML.Light.Input( parseXMLDoc )
import Text.XML.Light.Output( ppcTopElement, prettyConfigPP )


import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS

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
   arbitrary = oneof $ map return ["a", "b"]--genName

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

main filename cmd prop maxSuccess maxSize = let (prog, args) = (head spl, tail spl) in
    (case prop of
        "fuzz" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ fuzzprop filename prog args mencode)
        "check" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ checkprop filename prog args mencode)
        "gen" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ genprop filename prog args mencode)
    ) where spl = splitOn " " cmd

