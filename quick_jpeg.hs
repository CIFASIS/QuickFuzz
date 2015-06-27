{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

import Test.QuickCheck
import Test.QuickCheck.Instances

import Control.Monad.Zip
import Control.Exception
import Data.Binary( Binary(..), encode )

import Codec.Picture.Jpg.Types
import Codec.Picture.Tiff.Types
import Codec.Picture.Jpg.DefaultTable
import Codec.Picture.Metadata.Exif
import qualified Data.ByteString.Lazy as L
import Data.DeriveTH
import Data.Word(Word8, Word16, Word32)
import Data.Int( Int16, Int8 )

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS

import System.Process

instance Arbitrary (V.Vector Word32) where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Word32)
     return $ V.fromList l

instance Arbitrary (V.Vector Word16) where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Word16)
     return $ V.fromList l

instance Arbitrary (VU.Vector Int) where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Int)
     return $ VU.fromList l

instance Arbitrary (VU.Vector Int8) where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Int8)
     return $ VU.fromList l

instance Arbitrary (VU.Vector Word8) where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Word8)
     return $ VU.fromList l

instance Arbitrary (VS.Vector Word16) where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Word16)
     return $ VS.fromList l

instance Arbitrary (MacroBlock Int16) where
    arbitrary = do 
     l <- listOf (arbitrary :: Gen Int16)
     return $ VS.fromList l

instance Arbitrary (V.Vector (VU.Vector Word8)) where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Word8)
     return $ V.replicate 32 (VU.fromList l)

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

filenames = take 11 (repeat "buggy.jpg")

handler :: SomeException -> IO ()
handler _ = return ()
 
main = do
  jpgs  <- sample' (arbitrary :: Gen JpgImage)
  mapM_ (\(filename,jpg) -> 
      do
       catch (L.writeFile filename (encode jpg)) handler
       rawSystem "/usr/bin/jpeginfo" ["buggy.jpg"]
     ) (zip filenames jpgs)
  main
