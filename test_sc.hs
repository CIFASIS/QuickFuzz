{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

import Test.QuickCheck

import Test.SmallCheck
import Test.SmallCheck.Series

import Control.Exception
import Data.Binary( Binary(..), encode )

import qualified Data.ByteString.Lazy as L
--import Data.DeriveTH
--import Data.Word(Word8, Word16, Word32)
--import Data.Int( Int16, Int8 )
import GHC.Word

import System.Process
import System.Exit

import Codec.Picture.Gif
import Codec.Picture.Types
--import Codec.Picture.Types

import GHC.Generics
import Generics.Deriving
import Generics.Deriving.TH

import Control.Monad.Logic
import Control.Monad.Reader
import Control.Applicative
import Control.Monad.Identity
--import Data.ByteString.Internal
import qualified Data.ByteString as B

import qualified Data.Vector.Storable as VS

import System.Random

import Data.Char (ord)
import Data.List (inits)
import Data.Word (Word8)
import qualified Data.ByteString.Lazy.Char8 as L8 (pack)


--import Test.SmallCheck.Series.Types

--derive makeSerial ''GifFile
--derive makeSerial ''GifLooping
--derive makeArbitrary ''GifImage
--derive makeArbitrary ''GraphicControlExtension
--derive makeArbitrary ''GifHeader
--derive makeArbitrary ''DisposalMethod
--derive makeArbitrary ''LogicalScreenDescriptor
--derive makeArbitrary ''GifVersion
--derive makeArbitrary ''Image
--derive makeArbitrary ''PixelRGB8
--derive makeArbitrary ''ImageDescriptor
--derive makeArbitrary ''PixelBaseComponent



handler :: SomeException -> IO ()
handler _ = return () 

filenames = repeat "buggy.gif"

prop = do
  mapM_ (\(zipf) -> 
      do
       L.writeFile "buggy_sc.gif" (encode zipf)
       --rawSystem "/usr/bin/gifburst" ["buggy.gif"]
       r <- (randomIO :: IO Int)
       --putStrLn (show (zipf))
       ret <- rawSystem "/usr/bin/zzuf" ["-s", (show (r `mod` 1024))++":"++(show (r `mod` 1024 + 1)), "-r0", "-I", "x", "-c", "-S", "-T", "1", "./bins/test", "buggy_sc.gif"]
       --ret <- rawSystem "/usr/bin/valgrind" ["--quiet",  "./bins/test", "buggy_sc.gif"]
       case ret of
        ExitFailure x -> ( do 
                            putStrLn (show x) 
                            exitWith ret)
        _             -> return ()
 
     )  (listSeries 10 :: [GifFile])
  

deriving instance Generic GifFile
deriving instance Generic GifLooping
deriving instance Generic GifImage
deriving instance Generic GraphicControlExtension 
deriving instance Generic DisposalMethod 
deriving instance Generic GifHeader 
deriving instance Generic ImageDescriptor
deriving instance Generic LogicalScreenDescriptor
deriving instance Generic GifVersion


deriving instance Show GifFile
deriving instance Show GifLooping
deriving instance Show GifImage
deriving instance Show GraphicControlExtension 
deriving instance Show DisposalMethod 
deriving instance Show GifHeader 
deriving instance Show ImageDescriptor
deriving instance Show LogicalScreenDescriptor
deriving instance Show GifVersion
deriving instance Show Palette


 
--derive makeGeneric ''GifFile

instance Monad m => Serial m GifFile
instance Monad m => Serial m GifLooping
instance Monad m => Serial m GifImage
instance Monad m => Serial m GraphicControlExtension
instance Monad m => Serial m DisposalMethod
instance Monad m => Serial m GifHeader 
instance Monad m => Serial m ImageDescriptor
instance Monad m => Serial m LogicalScreenDescriptor 
instance Monad m => Serial m GifVersion
 
instance Monad m => Serial m B.ByteString where
   series = Test.SmallCheck.Series.generate (\d -> if d >= 0 then pure (B.replicate (d+1) 1) else empty)

instance Monad m => Serial m (Image PixelRGB8) where
   series = do
       return $ Image { imageWidth = 0xffffffff, imageHeight = 3, imageData = VS.fromList (listSeries 1 :: [Word8])}

--replicateW8 :: Word8 -> Series m ByteString
--replicateW8 b = generate $ \d -> fmap L8.pack . inits $ (replicate . fromIntegral) d b
 
instance Monad m => Serial m Word16 where
  series = 
    Test.SmallCheck.Series.generate (\d -> take (d+1) [0x00 .. 0xFFFF])

instance Monad m => Serial m Word8 where
  series =
    Test.SmallCheck.Series.generate (\d -> take (d+1) [0x00 .. 0xFF])

main = prop
