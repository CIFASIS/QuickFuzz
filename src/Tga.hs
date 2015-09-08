{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Tga where

import Test.QuickCheck
import Check

import Control.Monad.Zip
import Control.Exception
import Data.Binary( Binary(..), encode )

import Codec.Picture.Types
import Codec.Picture.Tga.Types
import Codec.Picture.Metadata.Exif
import Codec.Picture.Metadata

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

import Data.DeriveTH
import Data.Word(Word8, Word16, Word32)
import Data.Int( Int16, Int8 )

import DeriveArbitrary

import GHC.Types
import GHC.Word

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS

import System.Process
import System.Exit

import ByteString

import Data.List.Split



{-

instance Arbitrary a => Arbitrary (V.Vector a) where
   arbitrary = do 
     l <- listOf arbitrary
     return $ V.fromList l

instance (VU.Unbox a, Arbitrary a) => Arbitrary (VU.Vector a) where
   arbitrary = do 
     l <- listOf arbitrary
     return $ VU.fromList l

instance (VS.Storable a, Arbitrary a) => Arbitrary (VS.Vector a) where
   arbitrary = do 
     l <- listOf arbitrary
     return $ VS.fromList l


instance Arbitrary B.ByteString where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Word8)
     return $ B.pack l

instance Arbitrary L.ByteString where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Word8)
     return $ L.pack l
-}
{-
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
       return $ Image { imageWidth = w, imageHeight = h, imageData = VS.fromList [1] }

instance Show (Image PixelYCbCr8) where
   show x = ""
-}
{-
derive makeArbitrary ''ExifTag
derive makeArbitrary ''IfdType
derive makeArbitrary ''ExifData
derive makeArbitrary ''JpgAdobeApp14
derive makeArbitrary ''JFifUnit
derive makeArbitrary ''AdobeTransform

derive makeArbitrary ''JpgJFIFApp0
derive makeArbitrary ''ImageFileDirectory

derive makeArbitrary ''DctComponent
derive makeArbitrary ''JpgQuantTableSpec
derive makeArbitrary ''JpgHuffmanTableSpec
derive makeArbitrary ''JpgScanSpecification
derive makeArbitrary ''JpgScanHeader
derive makeArbitrary ''JpgFrameKind
derive makeArbitrary ''JpgComponent
derive makeArbitrary ''JpgFrameHeader
derive makeArbitrary ''JpgFrame
derive makeArbitrary ''JpgImage
derive makeArbitrary ''SourceFormat
-}
derive makeShow ''TgaFile_t
derive makeShow ''TgaImageType
derive makeShow ''TgaImageDescription
derive makeShow ''TgaColorMapType
derive makeShow ''TgaHeader

derive makeArbitrary ''TgaFile_t
$(deriveArbitraryRec ''TgaHeader)


mencode :: TgaFile_t -> L.ByteString
mencode = encode --(encode :: TgaFile -> L.ByteString) --JpgImage

main filename cmd prop maxSuccess maxSize = let (prog, args) = (head spl, tail spl) in
    (case prop of
        "fuzz" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ fuzzprop filename prog args mencode)
        "check" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ checkprop filename prog args mencode)
        "gen" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ genprop filename prog args mencode)
    ) where spl = splitOn " " cmd
