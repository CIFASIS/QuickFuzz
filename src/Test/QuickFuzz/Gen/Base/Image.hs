{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances, DeriveGeneric #-}

module Test.QuickFuzz.Gen.Base.Image where

import Control.DeepSeq

import Codec.Picture.Types
import Codec.Picture.Metadata
import Codec.Picture.ColorQuant
import Test.QuickCheck

import Data.Word(Word8, Word16, Word32)
import qualified Data.Vector.Storable as VS

import Test.QuickFuzz.Derive.Actions
import Test.QuickFuzz.Derive.Arbitrary

instance Arbitrary PixelRGBA8 where
  arbitrary = do
      (r,g,b,a) <- arbitrary
      return $ PixelRGBA8 r g b a

instance Show (Image PixelRGBA8) where
    show x = "Image RGBA8(" ++ show (imageWidth x)  ++ ", " ++  show (imageHeight x) ++ ")"


instance Arbitrary (Image Pixel8) where
   arbitrary = do
       l <- infiniteListOf (arbitrary :: Gen Pixel8)
       Positive w <- (arbitrary :: Gen (Positive Int))
       Positive h <- (arbitrary :: Gen (Positive Int))
       return $ Image { imageWidth = w, imageHeight = h, imageData = VS.fromList (take (3*w*h) l) }

instance Show (Image Pixel8) where
    show x = "Image Pixel8(" ++ show (imageWidth x) ++ ", " ++ show (imageHeight x) ++ ")"

instance Arbitrary (Image PixelRGB8) where
   arbitrary = do
       l <- infiniteListOf (arbitrary :: Gen Word8)
       Positive w <- (arbitrary :: Gen (Positive Int))
       Positive h <- (arbitrary :: Gen (Positive Int))
       return $ Image { imageWidth = w, imageHeight = h, imageData = VS.fromList (take (w*h) l) }

instance Show (Image PixelRGB8) where
    show x = "RGB8 Image(" ++ show (imageWidth x) ++ ", " ++ show (imageHeight x) ++ ")"

instance Arbitrary (Image PixelRGB16) where
   arbitrary = do
       l <- infiniteListOf (arbitrary :: Gen Word16)
       Positive w <- (arbitrary :: Gen (Positive Int))
       Positive h <- (arbitrary :: Gen (Positive Int))
       return $ Image { imageWidth = w, imageHeight = h, imageData = VS.fromList (take (w*h) l) }

instance Show (Image PixelRGB16) where
    show x = "RGB16 Image(" ++ show (imageWidth x) ++ ", " ++ show (imageHeight x) ++ ")" 

instance Arbitrary (Elem Keys) where
  arbitrary = do
               k <- (arbitrary :: Gen (Keys Word))
               w <- (arbitrary :: Gen (Word))
               return (k :=> w)

instance Arbitrary (Keys Word) where
  arbitrary = do 
                word <- (arbitrary :: Gen Word)
                oneof [return DpiX, return DpiY, return Width, return Height]

$(devActions ["Codec.Picture.Metadata"] ''Metadatas False [''Word] ['getMetadatas])
$(devArbitrary ''MetadatasAction)
$(devArbitraryWithActions False ''Metadatas)
