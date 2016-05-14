{-# LANGUAGE TemplateHaskell, FlexibleInstances, DeriveAnyClass, OverloadedStrings #-}

module AltIco where

import Images
import Png
import Test.QuickCheck

import Data.Binary( Binary(..), encode, decode )
import Data.Binary.Put
import Codec.Picture.Bitmap
import Codec.Picture.Types

import DeriveArbitrary
import Data.DeriveTH

import qualified Data.ByteString.Lazy as BL

import Data.Word (Word8, Word16, Word32)
import qualified Data.Vector.Storable as V

data IcoImage = Bmp (Image PixelRGBA8) (Image Pixel8) | Png MPngImage

data IcoType = Icon | Cursor deriving (Show, Read, Eq)

data IcoHeader = IcoHeader
  { --icoReserved :: !Word16
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

sizeofIcoHeader, sizeofIcoEntry :: Word32
sizeofIcoHeader = 6
sizeofIcoEntry = 16

type IcoFile = [IcoImage]

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


encodeImgData :: IcoImage -> Put
encodeImgData (Bmp x a) = bmpEncode x >> bmpEncode a
encodeImgData (Png i) = put i

getHeaders :: IcoFile -> Integer -> [(IcoEntry, BmpInfoHeader)]
getHeaders [] _ = []
getHeaders (i:imgs) off = case i of
                          Bmp x a -> let xWidth = fromIntegral $ imageWidth x
                                         xHeight = fromIntegral $ imageHeight x
                                         xSize = xWidth * xHeight * 4
                                         aWidth = fromIntegral $ imageWidth a
                                         aHeight = fromIntegral $ imageHeight a
                                         aSize = (fromIntegral $ xHeight) * 4
                                         ie = IcoEntry {
                                                icoWidth = fromIntegral $ xWidth,
                                                icoHeight = fromIntegral $ xHeight,
                                                icoColorCount = 0,
                                                icoPlanes = 1,
                                                icoBitCount = 32,
                                                icoSize = sizeofBmpInfo + (fromIntegral xSize) + (fromIntegral aSize),
                                                icoOffset = (fromIntegral $ off) + sizeofIcoEntry
                                              }
                                         bih = BmpInfoHeader {
                                                size = sizeofBmpInfo,
                                                width = fromIntegral $ xWidth,
                                                height = fromIntegral $ (xHeight + aHeight),
                                                planes = 1,
                                                bitPerPixel = 32,
                                                bitmapCompression = 0,
                                                byteImageSize = fromIntegral $ xSize,
                                                xResolution = 0,
                                                yResolution = 0,
                                                colorCount = 0,
                                                importantColours = 0
                                        }
                                    in (ie, bih):(getHeaders imgs (off + (toInteger sizeofIcoEntry) + (toInteger sizeofBmpInfo) + xSize + aSize))
                          _ -> getHeaders imgs off

encodeIco :: IcoFile -> BL.ByteString
encodeIco images = let
                     ih = IcoHeader {
                            icoType = Icon,
                            icoCount = fromIntegral $ length images
                          }
                     heads = getHeaders images (toInteger sizeofIcoHeader)
                     ies = map fst heads
                     bihs = map snd heads
                   in runPut $ put ih >> mapM_ put ies >> mapM_ put bihs >> mapM_ encodeImgData images

altmencode = encodeIco

andpixel :: Image Pixel8
andpixel = Image 1 1 (V.singleton 0)

andsquare :: Image Pixel8
andsquare = Image 2 2 (V.fromList [0, 0, 0, 0])

whitepixel :: Image PixelRGBA8
whitepixel = Image 1 1 (V.fromList [255, 255, 255, 255])

white2 :: Image PixelRGBA8
white2 = Image 2 2 (V.fromList [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255])

blackpixel :: Image PixelRGBA8
blackpixel = Image 1 1 (V.fromList [0, 0, 0, 255])

filepath :: FilePath
filepath = "/home/franco/ej2.ico"

redpixel :: Image PixelRGBA8
redpixel = Image 1 1 (V.fromList [255, 0, 0, 255])

square :: Image PixelRGBA8
square = Image 2 2 (V.fromList [0,0,255,255, 0,255,0,255, 255,0,0,255, 255,255,255,255]) --blue, green, red, white

ej1 :: IO ()
ej1 = let ico = [Bmp whitepixel andpixel]
          encoded = altmencode ico
      in BL.writeFile filepath encoded

ej2 :: IO ()
ej2 = let ico = [Bmp square andsquare]
          encoded = altmencode ico
      in BL.writeFile "/home/franco/sq2.ico" encoded

ej3 :: IO ()
ej3 = let ico = [Bmp white2 andsquare]
          encoded = altmencode ico
      in BL.writeFile "/home/franco/white.ico" encoded
