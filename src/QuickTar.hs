{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module QuickTar where

import Test.QuickCheck
--import Test.QuickCheck.Instances
import Check

import Control.Monad.Zip
import Control.Exception
import Data.Binary( Binary(..), encode )

import Codec.Archive.Tar.Types
import Codec.Archive.Tar.Write

import System.Posix.Types

import qualified Data.ByteString.Lazy as L
import Data.DeriveTH
import Data.Word(Word8, Word16, Word32)
import Data.Int( Int16, Int8 )

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS

import System.Process
import System.Exit


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


derive makeArbitrary ''Entry
derive makeArbitrary ''EntryContent
derive makeArbitrary ''TarPath
derive makeArbitrary ''LinkTarget

derive makeArbitrary ''Format
derive makeArbitrary ''Ownership
derive makeArbitrary ''Permissions


main = quickCheckWith stdArgs { maxSuccess = 12000000, maxSize = 50 } (absprop "buggy_qc.tar" "/bin/tar" ["-vf", "buggy_qc.tar"] mencode)


--instance Arbitrary Permissions where
--   arbitrary = do
--     w32 <- arbitrary :: Gen Word32
--     return $ CMode w32


--derive makeArbitrary ''Entry
--derive makeArbitrary ''CompressionMethod

filenames = take 11 (repeat "buggy.tar")

handler :: SomeException -> IO ()
handler _ = return ()
 
main = do
  zips  <- sample' (resize 5000 (arbitrary :: Gen [Entry]))
  mapM_ (\(filename,zipf) -> 
      do
       catch (L.writeFile filename (write zipf)) handler
       --ret <- rawSystem "/home/vagrant/.local/bin/honggfuzz" ["-q", "-N300", "-f", "buggy.zip", "--", "/usr/bin/unzip","-l","___FILE___"]
       ret <- rawSystem "/usr/bin/zzuf" ["-s", "0:1", "-C", "0", "-S", "-q", "-I","x", "-T", "3", "/usr/bin/tar", "-tf", "buggy.tar"]
       --ret <- rawSystem "/usr/bin/valgrind" ["--quiet", "--track-origins=yes", "/bin/tar", "-tf", "buggy.tar"]

       case ret of
        ExitFailure x  -> ( do 
                            putStrLn (show x) 
                            exitWith ret)
        _             -> return ()
       --return ()
 
     ) (zip filenames zips)
  main
