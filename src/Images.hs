{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module Images where

import Codec.Picture.Types
import Test.QuickCheck

import Data.Word(Word8, Word16, Word32)
--import Data.Int( Int16, Int8 )

--import qualified Data.Vector as V
--import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS

import GHC.Types

instance Arbitrary (Image PixelYCbCr8) where
   arbitrary = do
       l <- listOf (arbitrary :: Gen (PixelBaseComponent PixelYCbCr8))
       w <- (arbitrary :: Gen Int)
       h <- (arbitrary :: Gen Int)
       return $ Image { imageWidth = w, imageHeight = h, imageData = VS.fromList [1] }

instance Show (Image PixelYCbCr8) where
   show x = ""

instance Arbitrary (Image PixelRGB8) where
   arbitrary = do
       l <- listOf (arbitrary :: Gen Word8)
       w <- (arbitrary :: Gen Int)
       h <- (arbitrary :: Gen Int)
       return $ Image { imageWidth = w, imageHeight = h, imageData = VS.fromList l }

instance Show (Image PixelRGB8) where
   show x = ""
 
instance Arbitrary (Image PixelRGBA8) where
   arbitrary = do
       l <- listOf (arbitrary :: Gen Word8)
       w <- (arbitrary :: Gen Int)
       h <- (arbitrary :: Gen Int)
       return $ Image { imageWidth = w, imageHeight = h, imageData = VS.fromList l }

instance Show (Image PixelRGBA8) where
   show x = ""

