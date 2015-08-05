{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

import Test.QuickCheck
--import Test.QuickCheck.Gen

import Control.Monad.Zip
import Control.Exception
import Data.Binary( Binary(..), encode )

import Codec.Picture.Tiff
import Codec.Picture.Tiff.Types
import Codec.Picture.Types

import Codec.Picture.Metadata
import Codec.Picture.Metadata.Exif

import qualified Data.ByteString.Lazy as L
--import qualified Data.ByteString as B
import Data.DeriveTH
import Data.Word(Word8, Word16, Word32)
import Data.Int( Int16, Int8 )

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS

import Codec.Picture.VectorByteConversion( toByteString )

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

instance Arbitrary (V.Vector Word32) where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Word32)
     return $ V.fromList l

exifExample :: ExifTag -> Keys ExifData
exifExample TagCompression = (ExifShort :=> 1)


instance Arbitrary (Metadatas) where
  arbitrary = do --return $ Title :=> "abc"
      w <- (arbitrary :: Gen Word)
      s <- (arbitrary :: Gen String)
      d <- (arbitrary :: Gen Double) 
      sf <- (arbitrary :: Gen SourceFormat)
      return $ Metadatas { getMetadatas = [ Format :=> sf, Gamma :=> d,  DpiX :=> w, DpiY :=> w, Width :=> w, Height :=> w, Title :=> s, exifExample ] }

instance Arbitrary (Image PixelRGB8) where
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

type TiffFile  = (TiffInfo, Image PixelRGB8)

encodeTiffFile (hdr,img) = runPut $ putP rawPixelData hdr
                           where rawPixelData = toByteString $ imageData img

derive makeArbitrary ''TiffInfo
--derive makeArbitrary ''Metadatas
derive makeArbitrary ''Predictor
derive makeArbitrary ''ExtraSample
derive makeArbitrary ''TiffCompression
derive makeArbitrary ''TiffSampleFormat
derive makeArbitrary ''TiffPlanarConfiguration
derive makeArbitrary ''TiffColorspace
derive makeArbitrary ''TiffHeader
derive makeArbitrary ''Endianness
derive makeArbitrary ''SourceFormat


--derive makeArbitrary ''Elem
--derive makeArbitrary ''GraphicControlExtension
--derive makeArbitrary ''GifHeader
--derive makeArbitrary ''DisposalMethod
--derive makeArbitrary ''LogicalScreenDescriptor
--derive makeArbitrary ''GifVersion
--derive makeArbitrary ''Image
--derive makeArbitrary ''PixelRGB8
--derive makeArbitrary ''ImageDescriptor
--derive makeArbitrary ''PixelBaseComponent



filenames = take 1 (repeat "buggy_qc.tiff")

handler :: SomeException -> IO ()
handler _ = return ()
 
prop = do
  zips  <- sample' (resize 10 (arbitrary :: Gen TiffFile))
  mapM_ (\(filename,zipf) -> 
      do
       catch (L.writeFile filename (encodeTiffFile zipf)) handler
       --ret <- rawSystem "/home/vagrant/.local/bin/honggfuzz" ["-q", "-N3000", "-f", "buggy.dot", "--", "/usr/bin/dot","___FILE___"]
       --putStrLn (renderDot zipf)
       r <- (randomIO :: IO Int)
       --ret <- rawSystem "/usr/bin/zzuf" ["-s", (show (r `mod` 10024))++":"++(show (r `mod` 10024 + 10000)), "-j 5", "-q", "-M-1", "-c", "-S", "-T", "10", "/usr/bin/identify.im6", "buggy_qc.tiff"]
       ret <- rawSystem "/usr/bin/zzuf" ["-s", (show (r `mod` 10024))++":"++(show (r `mod` 10024 + 10000)), "-j 5", "-M-1", "-c", "-S", "-T", "10", "bins/test", "buggy_qc.tiff"]

       --ret <- rawSystem "/usr/bin/zzuf" ["-s", "0:100", "-c", "-S", "-T", "3", "/usr/bin/tiffinfo", "buggy_qc.tiff"]
       --ret <- rawSystem "/usr/bin/valgrind" ["--quiet",  "/usr/bin/tiffinfo", "buggy_qc.tiff"]
       case ret of
        --ExitFailure x -> ( do 
        --                    putStrLn (show x) 
        --                    exitWith ret)
        _             -> return ()
 
     ) (zip filenames zips)
  prop

main = prop
