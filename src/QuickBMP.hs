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

derive makeArbitrary ''SourceFormat
derive makeArbitrary ''BmpPalette
derive makeArbitrary ''BmpInfoHeader
derive makeArbitrary ''BmpHeader

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

instance Arbitrary (Image PixelRGBA8) where
   arbitrary = do
       l <- listOf (arbitrary :: Gen Word8)
       w <- (arbitrary :: Gen Int)
       h <- (arbitrary :: Gen Int)
       return $ Image { imageWidth = w, imageHeight = h, imageData = VS.fromList l }

instance Arbitrary (Image PixelRGB16) where
   arbitrary = do
       l <- listOf (arbitrary :: Gen Word16)
       w <- (arbitrary :: Gen Int)
       h <- (arbitrary :: Gen Int)
       return $ Image { imageWidth = w, imageHeight = h, imageData = VS.fromList l }

--type BMPFile  = (Metadatas, BmpPalette, Image PixelRGB8)
type BMPFile  = (BmpHeader, BmpInfoHeader, BmpPalette, Image PixelRGBA8)

--encodeBMPFile (metas,pal,img) = encodeBitmapWithPaletteAndMetadata metas pal img
encodeBMPFile (hdr, info, pal, img) = runPut $ put hdr >> put info >> putPalette pal >> bmpEncode img

filenames = take 1 (repeat "buggy_qc.bmp")

handler :: SomeException -> IO ()
handler _ = return ()
 
prop = do
  zips  <- sample' (resize 10 (arbitrary :: Gen BMPFile))
  mapM_ (\(filename,zipf) -> 
      do
       catch (L.writeFile filename (encodeBMPFile zipf)) handler
       --ret <- rawSystem "/home/vagrant/.local/bin/honggfuzz" ["-q", "-N3000", "-f", "buggy.dot", "--", "/usr/bin/dot","___FILE___"]
       --putStrLn (renderDot zipf)
       r <- (randomIO :: IO Int)
       ret <- rawSystem "/usr/bin/zzuf" ["-s", (show (r `mod` 10024))++":"++(show (r `mod` 10024 + 1000)), "-q", "-c", "-S", "-T", "10", "bins/test",  "buggy_qc.bmp"]
       --ret <- rawSystem "/usr/bin/zzuf" ["-s", "0:100", "-c", "-S", "-T", "3", "/usr/bin/tiffinfo", "buggy_qc.tiff"]
       --ret <- rawSystem "/usr/bin/valgrind" ["--quiet",  "bins/test", "buggy_qc.bmp"]
       case ret of
        ExitFailure x -> ( do 
                            putStrLn (show x) 
                            exitWith ret)
        _             -> return ()
 
     ) (zip filenames zips)
  prop

main = prop
