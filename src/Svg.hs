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

--instance  Arbitrary DT.Text where
--   arbitrary = do 
--     xs <- mgenName
--     oneof $ Prelude.map (return . T.pack) [xs]
  
instance Arbitrary String where
  arbitrary = mgenName

instance Arbitrary RPoint where
   arbitrary = do 
     a1 <- arbitrary
     a2 <- arbitrary
     return $ V2 a1 a2

$(devArbitrary ''MSvgFile)

encodeMSvgFile = LC8.pack . ppcTopElement prettyConfigPP . xmlOfDocument
   
mencode :: MSvgFile -> LC8.ByteString
mencode (MSvgFile d) = encodeMSvgFile d

mdecode :: C8.ByteString -> MSvgFile
mdecode x = 
           case (parseSvgFile "." x) of --(B.pack  $ Prelude.map (ord . toEnum) (C.unpack x))) of
             Just doc -> MSvgFile doc
             Nothing  -> error "SVG impossible to parse"
           --where x' = C8.unpack x 
