{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module QuickGif where

import Test.QuickCheck
import Check
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

import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Control.Monad.Reader()

instance Arbitrary B.ByteString where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Word8)
     return $ B.pack [1]--l

instance Arbitrary (V.Vector Word32) where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Word32)
     return $ V.fromList [1] --l

instance Arbitrary (V.Vector Word16) where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Word16)
     return $ V.fromList [1] --l

instance Arbitrary (VU.Vector Int) where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Int)
     return $ VU.fromList [1] --l

instance Arbitrary (VU.Vector Int8) where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Int8)
     return $ VU.fromList [1] --l

instance Arbitrary (VU.Vector Word8) where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Word8)
     return $ VU.fromList [1] --l

instance Arbitrary (VS.Vector Word16) where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Word16)
     return $ VS.fromList [1]--l

instance Arbitrary (V.Vector (VU.Vector Word8)) where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Word8)
     --x <- (arbitrary :: Gen Int)
     return $ V.replicate 32 (VU.fromList [1])


instance Arbitrary (Image PixelRGB8) where
   arbitrary = do
       l <- listOf (arbitrary :: Gen Word8)
       w <- (arbitrary :: Gen Int)
       h <- (arbitrary :: Gen Int)
       return $ Image { imageWidth = 16, imageHeight = 16, imageData = VS.fromList [1] }

instance Arbitrary (Image Word8) where
   arbitrary = do
       w <- (arbitrary :: Gen Int)
       h <- (arbitrary :: Gen Int)
       return $ Image { imageWidth = 16, imageHeight = 16, imageData = VS.fromList [1] }

instance Show (Image Word8) where
   show x = ""

derive makeArbitrary ''GifFile
derive makeShow ''GifFile
derive makeArbitrary ''GifLooping
derive makeShow ''GifLooping

derive makeArbitrary ''GifImage
derive makeArbitrary ''GraphicControlExtension
derive makeArbitrary ''GifHeader
derive makeArbitrary ''DisposalMethod
derive makeArbitrary ''LogicalScreenDescriptor
derive makeArbitrary ''GifVersion
derive makeArbitrary ''PixelRGB8
derive makeArbitrary ''ImageDescriptor

derive makeShow ''GifImage
derive makeShow ''GraphicControlExtension
derive makeShow ''GifHeader
derive makeShow ''DisposalMethod
derive makeShow ''LogicalScreenDescriptor
derive makeShow ''GifVersion
derive makeShow ''ImageDescriptor

instance Show Palette where
   show x = ""

--derive makeShow ''Palette

handler :: SomeException -> IO ()
handler _ = return ()

fromRight           :: Either a b -> b
fromRight (Right x)  = x
fromRight (Left x) = error "abc"

type MGifImage  = (Image Pixel8, Palette) 
encodeMGifImage :: MGifImage -> L.ByteString 
encodeMGifImage (img, pal) = fromRight $ encodeGifImageWithPalette img pal

mencode :: MGifImage -> L.ByteString 
mencode = encodeMGifImage

main = quickCheckWith stdArgs { maxSuccess = 12000000, maxSize = 20 } (absprop "buggy_qc.gif" "mplayer" ["buggy_qc.gif",  "-vo", "png"] mencode)
--main = quickCheckWith stdArgs { maxSuccess = 50000000, maxSize = 10 } absprop 

