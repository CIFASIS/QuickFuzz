{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module Images where

import Codec.Picture.Types
import Codec.Picture.Metadata
import Test.QuickCheck

import Data.Word(Word8, Word16, Word32)
import qualified Data.Vector.Storable as VS

import GHC.Types
import DeriveArbitrary

$(deriveArbitraryRec ''SourceFormat)

instance Arbitrary (Metadatas) where
  arbitrary = do
      w <- (arbitrary :: Gen Word)
      s <- (arbitrary :: Gen String)
      d <- (arbitrary :: Gen Double) 
      sf <- (arbitrary :: Gen SourceFormat)
      return $ Metadatas { getMetadatas = [ Format :=> sf, Gamma :=> d,  DpiX :=> w, DpiY :=> w, Width :=> w, Height :=> w, Title :=> s] }

instance Arbitrary (Image PixelYCbCr8) where
   arbitrary = do
       l <- listOf (arbitrary :: Gen (PixelBaseComponent PixelYCbCr8))
       w <- (arbitrary :: Gen Int)
       h <- (arbitrary :: Gen Int)
       return $ Image { imageWidth = w, imageHeight = h, imageData = VS.fromList l }

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

instance Arbitrary (Image Pixel8) where
   arbitrary = do
       l <- listOf (arbitrary :: Gen Word8)
       w <- (arbitrary :: Gen Int)
       h <- (arbitrary :: Gen Int)
       return $ Image { imageWidth = w, imageHeight = h, imageData = VS.fromList l }

instance Show (Image Pixel8) where
   show x = ""
 
 
instance Arbitrary (Image PixelRGBA8) where
   arbitrary = do
       l <- listOf (arbitrary :: Gen Word8)
       w <- (arbitrary :: Gen Int)
       h <- (arbitrary :: Gen Int)
       return $ Image { imageWidth = w, imageHeight = h, imageData = VS.fromList l }

instance Show (Image PixelRGBA8) where
   show x = ""

