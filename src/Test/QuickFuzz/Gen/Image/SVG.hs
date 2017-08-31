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
import Control.DeepSeq
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.List
import Data.Maybe
import Data.Monoid 

import Test.QuickFuzz.Derive.Arbitrary

import Test.QuickFuzz.Derive.Show
import Test.QuickFuzz.Derive.NFData

import Test.QuickFuzz.Gen.FormatInfo
import Test.QuickFuzz.Gen.Base.ByteString
import Test.QuickFuzz.Gen.Base.String

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L

import qualified Data.ByteString as BS


--import Data.Text (unpack)
--import Data.Text.Encoding (decodeUtf8)

instance Arbitrary PixelRGBA8 where
  arbitrary = do
      (r,g,b,a) <- arbitrary
      return $ PixelRGBA8 r g b a

devArbitrary ''Tree

instance NFData PixelRGBA8 where
    rnf _ = ()

instance NFData Graphics.Svg.Types.Image where
    rnf _ = ()

devNFData ''Tree

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

getElements (Document { _elements = ts }) = ts 

--rdDocument bs = let fromJust (parseSvgFile "." bs)
--                in f (Just x) = x

svgInfo :: FormatInfo [Tree] NoActions
svgInfo = def 
    { encode = L8.pack . ppcTopElement prettyConfigPP . xmlOfDocument . mkDocument
    , decode = getElements . fromJust . (parseSvgFile ".") . BS.pack . L.unpack -- . L8.pack . L.unpack 
    , random = arbitrary
    , value = show
    , ext = "svg" 
    } 
