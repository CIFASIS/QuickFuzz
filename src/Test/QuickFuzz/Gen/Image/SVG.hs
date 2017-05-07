{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.QuickFuzz.Gen.Image.SVG where

import Data.Default

import Graphics.Svg
import Graphics.Svg.Types
import Codec.Picture.Types

import Text.XML.Light.Input( parseXMLDoc )
import Text.XML.Light.Output( ppcTopElement, prettyConfigPP )

import Test.QuickCheck
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.List
import Data.Monoid 

import Test.QuickFuzz.Derive.Arbitrary

import Test.QuickFuzz.Derive.Show
import Test.QuickFuzz.Gen.FormatInfo
import Test.QuickFuzz.Gen.Base.ByteString
import Test.QuickFuzz.Gen.Base.String

import qualified Data.ByteString.Lazy.Char8 as L8

instance Arbitrary PixelRGBA8 where
  arbitrary = do
      (r,g,b,a) <- arbitrary
      return $ PixelRGBA8 r g b a

devArbitrary ''Tree
-- devShow ''	

mkDocument ts = Document { _viewBox= Just (0.0,0.0,128.0,128.0), 
                           _width = Just (Num 128.0), 
                           _height = Just (Num 128.0),  
                           _definitions= mempty, 
                           _elements = ts,
                           _description = "", 
                           _styleRules = [], 
                           _documentLocation="." 
                          }

svgInfo :: FormatInfo [Tree] NoActions
svgInfo = def 
    { encode = L8.pack . ppcTopElement prettyConfigPP . xmlOfDocument . mkDocument
    , random = arbitrary
    , value = show
    , ext = "svg" 
    } 
