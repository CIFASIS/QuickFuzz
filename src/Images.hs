{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module Images where

import Codec.Picture.Types
import Codec.Picture.Metadata
import Codec.Picture.ColorQuant
import Test.QuickCheck

import Data.Word(Word8, Word16, Word32)
import qualified Data.Vector.Storable as VS

import GHC.Types

import DeriveArbitrary
import DeriveMArbitrary

--instance Arbitrary (Elem a) where
--  arbitrary = return $ undefined

--instance (Arbitrary a) => Arbitrary (Keys a) where
--  arbitrary = return $ undefined

-- $(devActions "Codec.Picture.Gif" ''Palette False [])
-- $(devArbitrary ''PaletteAction)
-- $(devArbitraryWithActions False ''Metadatas)



-- $(devActions "Codec.Picture.Metadata" ''Metadatas False [''Int])
-- $(devArbitrary ''MetadatasAction)
-- $(devArbitraryWithActions False ''Metadatas)


-- $(deriveArbitraryRec ''SourceFormat)

instance Arbitrary PixelRGBA8 where
  arbitrary = do
      (r,g,b,a) <- (arbitrary)
      return $ PixelRGBA8 r g b a

 
instance Arbitrary (Metadatas) where
  arbitrary = do
      w <- (arbitrary :: Gen Word)
      h <- (arbitrary :: Gen Word)
      dx <- (arbitrary :: Gen Word)
      dy <- (arbitrary :: Gen Word)

      s <- (arbitrary :: Gen String)
      d <- (arbitrary :: Gen Double) 
      --sf <- (arbitrary :: Gen SourceFormat)
      return $ Metadatas { getMetadatas = [Gamma :=> d,  DpiX :=> dx, DpiY :=> dy, Width :=> w, Height :=> h, Title :=> ""] }

instance Arbitrary (Image PixelYCbCr8) where
   arbitrary = do
       xs <- infiniteListOf (arbitrary :: Gen (PixelBaseComponent PixelYCbCr8))
       Positive w <- (arbitrary :: Gen (Positive Int))
       Positive h <- (arbitrary :: Gen (Positive Int))
       return $ Image { imageWidth = w, imageHeight = h, imageData = VS.fromList (take (3*w*h) xs) }

instance Show (Image PixelYCbCr8) where
   show x = ""


instance Arbitrary (Image PixelCMYK16) where
   arbitrary = do
       xs <- infiniteListOf (arbitrary :: Gen (PixelBaseComponent PixelCMYK16))
       Positive w <- (arbitrary :: Gen (Positive Int))
       Positive h <- (arbitrary :: Gen (Positive Int))
       return $ Image { imageWidth = w, imageHeight = h, imageData = VS.fromList (take (w*h) xs) }

instance Show (Image PixelCMYK16) where
   show x = ""

--PixelCMYK16

instance Arbitrary (Image PixelRGB8) where
   arbitrary = do
       l <- infiniteListOf (arbitrary :: Gen Word8)
       Positive w <- (arbitrary :: Gen (Positive Int))
       Positive h <- (arbitrary :: Gen (Positive Int))
       return $ Image { imageWidth = w, imageHeight = h, imageData = VS.fromList (take (3*w*h) l) }

instance Show (Image PixelRGB8) where
   show x = ""

instance Arbitrary (Image Pixel8) where
   arbitrary = do
       --l <- listOf (arbitrary :: Gen Word8)
       xs <- infiniteListOf (arbitrary :: Gen Word8)
       Positive w <- (arbitrary :: Gen (Positive Int))
       Positive h <- (arbitrary :: Gen (Positive Int))
       return $ Image { imageWidth = w, imageHeight = h, imageData = VS.fromList (take (3*w*h) xs)}

instance Show (Image Pixel8) where
   show x = ""
 
 
instance Arbitrary (Image PixelRGBA8) where
   arbitrary = do
       xs <- infiniteListOf (arbitrary :: Gen Word8)
       Positive w <- (arbitrary :: Gen (Positive Int))
       Positive h <- (arbitrary :: Gen (Positive Int))
       return $ Image { imageWidth = w, imageHeight = h, imageData = VS.fromList (take (4*w*h) xs) }

instance Show (Image PixelRGBA8) where
   show x = ""

instance Arbitrary (Image PixelRGB16) where
   arbitrary = do
       l <- listOf (arbitrary :: Gen (PixelBaseComponent PixelRGB16))
       w <- (arbitrary :: Gen Int)
       h <- (arbitrary :: Gen Int)
       return $ Image { imageWidth = w, imageHeight = h, imageData = VS.fromList l}

instance Show (Image PixelRGB16) where
   show x = ""

-- $(deriveArbitraryRec ''PaletteOptions)
instance Arbitrary (Image PixelYA8) where
   arbitrary = do
       --l <- listOf (arbitrary :: Gen Word8)
       xs <- infiniteListOf (arbitrary :: Gen Word8)
       Positive w <- (arbitrary :: Gen (Positive Int))
       Positive h <- (arbitrary :: Gen (Positive Int))
       return $ Image { imageWidth = w, imageHeight = h, imageData = VS.fromList (take (3*w*h) xs)}

instance Show (Image PixelYA8) where
   show x = ""


-- $(deriveArbitraryRec ''PaletteOptions)

-- derive makeShow ''PaletteOptions
-- derive makeShow ''PaletteCreationMethod
