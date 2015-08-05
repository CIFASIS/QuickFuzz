{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

import Test.QuickCheck

import Control.Monad.Zip
import Control.Exception
import Data.Binary( Binary(..), encode )

import System.Posix.Types

import qualified Data.ByteString.Lazy as L
import Data.DeriveTH
import Data.Word(Word8, Word16, Word32)
import Data.Int( Int16, Int8 )

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS

import Codec.Picture.Png.Type
import Codec.Picture.Png.Export
import Codec.Picture.Png
import Codec.Picture.Types
import Codec.Picture.Metadata

import GHC.Types

import System.Process
import System.Exit

import System.Random
  ( RandomGen(..)
  , Random(..)
  , StdGen
  , newStdGen
  )

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

instance Arbitrary (V.Vector (VU.Vector Word8)) where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Word8)
     return $ V.replicate 32 (VU.fromList l)

instance Arbitrary (Image Pixel8) where
   arbitrary = do
       l <- listOf (arbitrary :: Gen Word8)
       w <- (arbitrary :: Gen Int)
       h <- (arbitrary :: Gen Int)
       return $ Image { imageWidth = w, imageHeight = h, imageData = VS.fromList l }



instance Arbitrary (Image PixelRGB8) where
   arbitrary = do
       l <- listOf (arbitrary :: Gen Word8)
       w <- (arbitrary :: Gen Int)
       h <- (arbitrary :: Gen Int)
       return $ Image { imageWidth = w, imageHeight = h, imageData = VS.fromList l }

instance Arbitrary (Metadatas) where
  arbitrary = do --return $ Title :=> "abc"
      w <- (arbitrary :: Gen Word)
      s <- (arbitrary :: Gen String)
      d <- (arbitrary :: Gen Double) 
      sf <- (arbitrary :: Gen SourceFormat)
      return $ Metadatas { getMetadatas = [ Format :=> sf, Gamma :=> d,  DpiX :=> w, DpiY :=> w, Width :=> w, Height :=> w, Title :=> s] }


derive makeArbitrary ''SourceFormat

--derive makeArbitrary ''Permissions


--instance Arbitrary Permissions where
--   arbitrary = do
--     w32 <- arbitrary :: Gen Word32
--     return $ CMode w32

--derive makeArbitrary ''Entry
--derive makeArbitrary ''CompressionMethod

filenames = take 11 (repeat "buggy_qc.png")

type PngFile  = (Metadatas, Palette, Image Pixel8)

encodePngFile (metas, pal,img) = genericEncodePng (Just pal) PngIndexedColor metas img --encodePalettedPngWithMetadata meta pal img

handler :: SomeException -> IO ()
handler _ = return ()
 
main = do
  zips  <- sample' (resize 20 (arbitrary :: Gen PngFile))
  mapM_ (\(filename,zipf) -> 
      do
       catch (L.writeFile filename (encodePngFile zipf)) handler
       --ret <- rawSystem "/home/vagrant/.local/bin/honggfuzz" ["-q", "-N300", "-f", "buggy.zip", "--", "/usr/bin/unzip","-l","___FILE___"]
       --ret <- rawSystem "/usr/bin/zzuf" ["-s", "0:1", "-S", "-T", "3", "/usr/bin/convert.im6", "buggy.png", "/tmp/hola.bmp"]
       r <- (randomIO :: IO Int)
       ret <- rawSystem "/usr/bin/zzuf" ["-s", (show (r `mod` 10024))++":"++(show (r `mod` 10024 + 1)), "-r0", "-Ix",  "-M-1", "-c", "-S", "-T", "10", "/usr/bin/convert.im6", "buggy_qc.png", "jpeg:-"]
       
       --ret <- rawSystem "/usr/bin/valgrind" ["--quiet", "--track-origins=yes", "/bin/tar", "-tf", "buggy.tar"]

       case ret of
        ExitFailure x  -> ( do 
                            putStrLn (show x) 
                            exitWith ret)
        _             -> return ()
       --return ()
 
     ) (zip filenames zips)
  main
