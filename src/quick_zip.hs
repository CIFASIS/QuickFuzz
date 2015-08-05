{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

import Test.QuickCheck
--import Test.QuickCheck.Instances

import Control.Monad.Zip
import Control.Exception
import Data.Binary( Binary(..), encode )

import Codec.Archive.Zip

import qualified Data.ByteString.Lazy as L
import Data.DeriveTH
import Data.Word(Word8, Word16, Word32)
import Data.Int( Int16, Int8 )

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS

import System.Process
import System.Exit
import System.Posix

import System.Random

getFileSize :: String -> IO FileOffset
getFileSize path = do
    stat <- getFileStatus path
    return (fileSize stat)

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

instance Arbitrary L.ByteString where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Word8)
     return $ L.pack l



derive makeArbitrary ''Archive
derive makeArbitrary ''Entry
derive makeArbitrary ''CompressionMethod


{-
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
-}

filenames = take 11 (repeat "buggy.zip")

handler :: SomeException -> IO ()
handler _ = return ()
 
main = do
  zips  <- sample' (resize 100 (arbitrary :: Gen Archive))
  mapM_ (\(filename,zipf) -> 
      do
       catch (L.writeFile filename (encode zipf)) handler
       r <- (randomIO :: IO Int) 
       --rawSystem "/home/vagrant/.local/bin/radamsa" ["-o", "buggy.zip", "buggy_.zip"]
       --ret <- rawSystem "/home/vagrant/.local/bin/honggfuzz" ["-q", "-N300", "-f", "buggy.zip", "--", "/usr/bin/unzip","-l","___FILE___"]
       ret <- rawSystem "/usr/bin/zzuf" ["-s", (show (r `mod` 1024))++":"++(show (r `mod` 1024 + 10)), "-c", "-S" ,"-T", "3", "/usr/bin/advzip", "-l", "buggy.zip"]
       --ret <- rawSystem "/usr/bin/valgrind" ["--log-file=unzip.log", "--quiet", "/usr/bin/advzip", "-l", "buggy.zip"]
       --size <- getFileSize "unzip.log"
       --if size > 0 then exitWith (ExitFailure 1) else return ()

       case ret of
       -- (n+1 -> ( do 
       --           putStrLn (show size) 
       --                     exitWith 0)
       --
        --ExitFailure x -> ( do 
        --                   putStrLn (show x) 
        --                  exitWith ret)
        _             -> return ()
       --return ()
 
     ) (zip filenames zips)
  main
