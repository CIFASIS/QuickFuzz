{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

import Test.QuickCheck
--import Test.QuickCheck.Gen

import Control.Monad.Zip
import Control.Exception
import Data.Binary( Binary(..), encode )

import Codec.Picture.Gif
import Codec.Picture.Types

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Data.DeriveTH
import Data.Word(Word8, Word16, Word32)
import Data.Int( Int16, Int8 )

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS

import System.Process
import System.Exit

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

instance Arbitrary B.ByteString where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Word8)
     return $ B.pack l

--instance Arbitrary L.ByteString where
--   arbitrary = do 
--     l <- listOf (arbitrary :: Gen Word8)
--     return $ L.pack l

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


instance Arbitrary (Image PixelRGB8) where
   arbitrary = do
       l <- listOf (arbitrary :: Gen Word8)
       w <- return 65500 -- (arbitrary :: Gen Int)
       h <- return 65500 --(arbitrary :: Gen Int)
       return $ Image { imageWidth = w, imageHeight = h, imageData = VS.fromList l }

--instance Arbitrary (DisposalMethod) where
--   arbitrary = return $ DisposalDoNot 

--instance Arbitrary String where
--    arbitrary = vectorOf 5 (oneof $ map return "abc")


--derive makeArbitrary ''Graph
derive makeArbitrary ''GifFile
derive makeArbitrary ''GifLooping
derive makeArbitrary ''GifImage
derive makeArbitrary ''GraphicControlExtension
derive makeArbitrary ''GifHeader
derive makeArbitrary ''DisposalMethod
derive makeArbitrary ''LogicalScreenDescriptor
derive makeArbitrary ''GifVersion
--derive makeArbitrary ''Image
derive makeArbitrary ''PixelRGB8
derive makeArbitrary ''ImageDescriptor
--derive makeArbitrary ''PixelBaseComponent



filenames = take 1 (repeat "buggy_qc.gif")

handler :: SomeException -> IO ()
handler _ = return ()
 
prop = do
  zips  <- sample' (resize 10 (arbitrary :: Gen GifFile))
  mapM_ (\(filename,zipf) -> 
      do
       catch (L.writeFile filename (encode zipf)) handler
       --ret <- rawSystem "/home/vagrant/.local/bin/honggfuzz" ["-q", "-N3000", "-f", "buggy.dot", "--", "/usr/bin/dot","___FILE___"]
       --putStrLn (renderDot zipf)
       --ret <- rawSystem "/usr/bin/zzuf" ["-s", "0:1", "-r0", "-c", "-S", "-I", "x","-T", "3", "/usr/bin/gif2png", "buggy_qc.gif"]
       r <- (randomIO :: IO Int)
       --ret <- rawSystem "/usr/bin/zzuf" ["-s", (show (r `mod` 10024))++":"++(show (r `mod` 10024 + 100)), "-q", "-M-1", "-c", "-S", "-T", "1", "/usr/bin/convert.im6", "buggy_qc.gif", "-resize", "128x128", "jpeg:-"]

       ret <- rawSystem "/usr/bin/zzuf" ["-s", (show (r `mod` 10024))++":"++(show (r `mod` 10024 + 100)), "-M-1", "-c", "-S", "-T", "1", "bins/test", "buggy_qc.gif"]
 

 
       --ret <- rawSystem "/usr/bin/valgrind" ["--quiet",  "/usr/bin/gif2png", "buggy_qc.gif"]
       case ret of
        --ExitFailure x -> ( do 
        --                    putStrLn (show x) 
        --                    exitWith ret)
        _             -> return ()
 
     ) (zip filenames zips)
  prop

main = prop
