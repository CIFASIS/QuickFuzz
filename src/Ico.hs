{-# LANGUAGE TemplateHaskell, FlexibleInstances, DeriveAnyClass, OverloadedStrings #-}

module Ico where

import Images
import Test.QuickCheck

import Data.Binary( Binary(..), encode )
import Data.Binary.Put
import Codec.Picture.Bitmap
import Codec.Picture.Types

import DeriveArbitrary
import Data.DeriveTH

import qualified Data.ByteString.Lazy as BL

import Data.Word (Word8, Word16, Word32)
import qualified Data.Vector.Storable as V


data IcoType = Icon | Cursor deriving (Show, Read, Eq)

data IcoHeader = IcoHeader
  { -- icoReserved :: !Word16
    icoType     :: IcoType
  , icoCount    :: !Word16
  } deriving (Show, Read, Eq)

data IcoEntry = IcoEntry
  { icoWidth      :: !Word8
  , icoHeight     :: !Word8
  , icoColorCount :: !Word8
  --, icoEReserved  :: !Word8
  , icoPlanes     :: !Word16
  , icoBitCount   :: !Word16
  , icoSize       :: !Word32
  , icoOffset     :: !Word32
  } deriving (Show, Read, Eq)

--data IcoFile = IcoFile IcoHeader [IcoEntry] [([PixelRGBA8], [Pixel8])] deriving Show--(xor, and); and is 1bit (different type?)
data IcoFile = IcoFile IcoHeader [IcoEntry] [(BmpInfoHeader, Image PixelRGBA8, Image Pixel8)] deriving Show

{-
instance Arbitrary IcoFile where
  arbitrary = sameLength

sameLength :: Gen IcoFile
sameLength = do
                h <- arbitrary :: Gen IcoHeader
                let ic = icoCount h
                es <- vector (fromIntegral $ toInteger ic)
                bs <- vector (fromIntegral $ toInteger ic)
                return $ IcoFile h es bs
-}

$(devArbitrary ''IcoFile)
--restrictions on arbitrary?

instance Binary IcoType where
  put Icon = putWord16le 1
  put Cursor = putWord16le 2
  get = undefined

instance Binary IcoHeader where
  put header = do
                  putWord16le $ 0 --icoReserved header
                  put $ icoType header
                  putWord16le $ icoCount header
  get = undefined

instance Binary IcoEntry where
  put entry = do
                  putWord8 $ icoWidth entry
                  putWord8 $ icoHeight entry
                  putWord8 $ icoColorCount entry
                  putWord8 $ 0--icoEReserved entry
                  putWord16le $ icoPlanes entry
                  putWord16le $ icoBitCount entry
                  putWord32le $ icoSize entry
                  putWord32le $ icoOffset entry
  get = undefined

--instance {-# OVERLAPPING #-} Binary [IcoEntry] where
--  put xs = mapM_ put xs
--  get = undefined

--instance {-# OVERLAPPING #-} Binary [PixelRGBA8] where
--  put xs = mapM_ put xs
--  get = undefined

--instance {-# OVERLAPPING #-} Binary [Pixel8] where
--  put xs = mapM_ put xs
--  get = undefined

--instance Binary PixelRGBA8 where
--  put (PixelRGBA8 r g b a) = put r >> put g >> put b >> put a
--  get = undefined

--empty lists?
{-instance Binary IcoFile where
  put (IcoFile h es bs) = do
                            put h
                            put es
                            put bs-}
--  put (IcoFile h es bs) = do
--                            put h
--                            put' es
--                            put'' bs
--                          where  put' []      = put (IcoEntry 0 0 0 0 0 0 0 0)
--                                 put' (e:es)  = put e >> put' es
--                                 put'' []     = put ???
--                                 put'' (b:bs) = put b >> put'' bs

--  get = undefined


encodeImgData :: (BmpInfoHeader, Image PixelRGBA8, Image Pixel8) -> Put
encodeImgData (ih, xbmp, ibmp) = put ih >> bmpEncode xbmp >> bmpEncode ibmp

encodeIco :: IcoFile -> BL.ByteString
encodeIco (IcoFile h es bs) = runPut $ put h >> (mapM_ put es) >> mapM_ encodeImgData bs

mencode = encodeIco

andmask :: Image Pixel8
andmask = Image 0 0 V.empty

whitepixel :: Image PixelRGBA8
whitepixel = Image 1 1 (V.fromList [255, 255, 255, 255])

blackpixel :: Image PixelRGBA8
blackpixel = Image 1 1 (V.fromList [0, 0, 0, 255])

filepath :: FilePath
filepath = "/home/franco/ej.ico"

square :: Image PixelRGBA8
square = Image 2 2 (V.fromList [0,0,255,255, 255,255,255,255, 0,255,0,255, 255,0,0,255]) --red, white, blue, gree

ej1 :: IO ()
ej1 = let ih        = IcoHeader {icoType = Icon, icoCount = 1}
          eh        = [IcoEntry {icoWidth = 1, icoHeight = 1, icoColorCount = 0, icoPlanes  = 0, icoBitCount = 32, icoSize = 44, icoOffset = 22} ]
          bh        = BmpInfoHeader {size = 40, width = 1, height = 1, planes = 1, bitPerPixel = 32, bitmapCompression = 0, byteImageSize = 4, xResolution = 0, yResolution = 0, colorCount = 0, importantColours = 0}
          bm        = [(bh, whitepixel, andmask)]
          ico       = IcoFile ih eh bm
          encoded   = mencode ico
      in BL.writeFile filepath encoded

ej2 :: IO ()
ej2 = let ih        = IcoHeader {icoType = Icon, icoCount = 1}
          eh        = [IcoEntry {icoWidth = 2, icoHeight = 2, icoColorCount = 0, icoPlanes  = 0, icoBitCount = 32, icoSize = 56, icoOffset = 22} ]
          bh        = BmpInfoHeader {size = 40, width = 2, height = 2, planes = 1, bitPerPixel = 32, bitmapCompression = 0, byteImageSize = 16, xResolution = 0, yResolution = 0, colorCount = 0, importantColours = 0}
          bm        = [(bh, square, andmask)]
          ico       = IcoFile ih eh bm
          encoded   = mencode ico
      in BL.writeFile "/home/franco/sq.ico" encoded
