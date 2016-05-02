{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module Svg where

import Test.QuickCheck

--import Data.Binary( Binary(..), encode )
import Graphics.Svg
import Graphics.Svg.Types

import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Data.DeriveTH

import DeriveMutation
import DeriveArbitrary

import ByteString
import Vector

import Text.XML.Light.Input( parseXMLDoc )
import Text.XML.Light.Output( ppcTopElement, prettyConfigPP )

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
import Strings
import Images 

data MSvgFile  = MSvgFile Document deriving Show

instance Mutation Char where
    mutt x = return x

instance  Mutation String where
    mutt = return

instance  Mutation DT.Text where
    mutt = return
 
--instance Arbitrary (Maybe String) where
--   arbitrary = return Nothing

instance  Arbitrary DT.Text where
   arbitrary = do 
     xs <- mgenName
     oneof $ Prelude.map (return . T.pack) [xs]--["a", "b", "c", "d", "e"]--genName
  
instance Arbitrary String where
  arbitrary = mgenName

instance Arbitrary RPoint where
   arbitrary = do 
     a1 <- arbitrary
     a2 <- arbitrary
     return $ V2 a1 a2

instance ProbGen RPoint where
    prob_gen xs n = V2 <$> prob_gen xs n <*> prob_gen xs n

instance ProbGen Coord where
    prob_gen _ _ = arbitrary

$(devArbitrary ''MSvgFile)
-- $(devMutationRec ''MSvgFile)

$(instaGen ''T.Text)
$(devIntGen ''MSvgFile)
-- $(devMutationRec ''MXml)

mgen :: [Int] -> Gen MSvgFile
mgen = undefined --customGen_Svg_MSvgFile
 
encodeMSvgFile = LC8.pack . ppcTopElement prettyConfigPP . xmlOfDocument
   
mencode :: MSvgFile -> LC8.ByteString
mencode (MSvgFile d) = encodeMSvgFile d

mdecode :: C8.ByteString -> MSvgFile
mdecode x = 
           case (parseSvgFile "." x) of --(B.pack  $ Prelude.map (ord . toEnum) (C.unpack x))) of
             Just doc -> MSvgFile doc
             Nothing  -> error "SVG impossible to parse"
           --where x' = C8.unpack x 
