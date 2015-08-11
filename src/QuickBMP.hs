{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}
module QuickBMP where

import Test.QuickCheck
--import Test.QuickCheck.Gen

import Control.Monad.Zip
import Control.Exception
import Data.Binary( Binary(..), encode )

import Codec.Picture.Bitmap
import Codec.Picture.Types

import Codec.Picture.Metadata

import qualified Data.ByteString.Lazy as L

import Data.DeriveTH
import Data.Word(Word8, Word16, Word32)
import Data.Int( Int16, Int8 )

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS

import System.Process
import System.Exit

import Data.Binary.Put( runPut )

import GHC.Types

import System.Random
  ( RandomGen(..)
  , Random(..)
  , StdGen
  , newStdGen
  )

import Control.Monad
  ( liftM
  )

import Control.Monad.Reader()

import Test.QuickCheck.Monadic (assert, monadicIO, run)



-- $( derive makeArbitrary ''BmpHeader, ''BmpInfoHeader )

derive makeArbitrary ''SourceFormat
derive makeArbitrary ''BmpPalette
derive makeArbitrary ''BmpInfoHeader
derive makeArbitrary ''BmpHeader

--derive makeShow ''SourceFormat
derive makeShow ''BmpPalette
--derive makeShow ''BmpInfoHeader
derive makeShow ''BmpHeader




instance Arbitrary (V.Vector Word32) where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Word32)
     return $ V.fromList l
{-
instance Arbitrary (Metadatas) where
  arbitrary = do --return $ Title :=> "abc"
      w <- (arbitrary :: Gen Word)
      s <- (arbitrary :: Gen String)
      d <- (arbitrary :: Gen Double) 
      sf <- (arbitrary :: Gen SourceFormat)
      return $ Metadatas { getMetadatas = [ Format :=> sf, Gamma :=> d,  DpiX :=> w, DpiY :=> w, Width :=> w, Height :=> w, Title :=> s] }
-}
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
 

instance Arbitrary (Image PixelRGB16) where
   arbitrary = do
       l <- listOf (arbitrary :: Gen Word16)
       w <- (arbitrary :: Gen Int)
       h <- (arbitrary :: Gen Int)
       return $ Image { imageWidth = w, imageHeight = h, imageData = VS.fromList l }

{-
instance Arbitrary (Image pixel0) where
   arbitrary = do
       l <- listOf (arbitrary :: Gen pixel0)
       w <- (arbitrary :: Gen Int)
       h <- (arbitrary :: Gen Int)
       return $ Image { imageWidth = w, imageHeight = h, imageData = VS.fromList l }

instance Show (Image pixel0) where
   show x = ""
-}

--type BMPFile  = (Metadatas, BmpPalette, Image PixelRGB8)
type BMPFile  = (BmpHeader, BmpInfoHeader, BmpPalette, Image PixelRGBA8)

--encodeBMPFile (metas,pal,img) = encodeBitmapWithPaletteAndMetadata metas pal img
encodeBMPFile (hdr, info, pal, img) = runPut $ put hdr >> put info >> putPalette pal >> bmpEncode img

filenames = take 1 (repeat "buggy_qc.bmp")

handler :: SomeException -> IO ()
handler _ = return ()

prop :: BMPFile -> Property
prop x = monadicIO $ do
         run $ Control.Exception.catch (L.writeFile "buggy_qc.bmp" (encodeBMPFile x)) handler
         r <- run (randomIO :: IO Int)
         ret <- run $ rawSystem "/usr/bin/zzuf" ["-s", (show (r `mod` 10024))++":"++(show (r `mod` 10024 + 50)), "-q", "-M", "-1", "-c", "-S", "-T", "60", "-j", "5", "/usr/bin/convert.im6", "-rotate", "90", "buggy_qc.bmp","png:-"]
         case ret of
            ExitFailure y -> Test.QuickCheck.Monadic.assert False 
            _             -> Test.QuickCheck.Monadic.assert True

main = quickCheckWith stdArgs { maxSuccess = 50000, maxSize = 500 } prop 
