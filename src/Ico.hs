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

--Ico0 generates "garbage", Ico1 should generate good icons
data IcoFile = Ico0 IcoHeader [IcoEntry] [(BmpInfoHeader, Image PixelRGBA8, Image Pixel8)] | Ico1 [(Image PixelRGBA8, Image Pixel8)] deriving Show

sizeofIcoHeader, sizeofIcoEntry :: Word32
sizeofIcoHeader = 6
sizeofIcoEntry = 16

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
                  putWord8 $ 0 --icoEReserved entry
                  putWord16le $ icoPlanes entry
                  putWord16le $ icoBitCount entry
                  putWord32le $ icoSize entry
                  putWord32le $ icoOffset entry
  get = undefined

encodeImgDataWithHeader :: (BmpInfoHeader, Image PixelRGBA8, Image Pixel8) -> Put
encodeImgDataWithHeader (ih, xbmp, abmp) = put ih >> bmpEncode xbmp >> bmpEncode abmp

encodeImgData :: (Image PixelRGBA8, Image Pixel8) -> Put
encodeImgData (xbmp, abmp) = bmpEncode xbmp >> bmpEncode abmp

getHeaders :: [(Image PixelRGBA8, Image Pixel8)] -> Integer -> [(IcoEntry, BmpInfoHeader)]
getHeaders [] _ = []
getHeaders ((x, a):imgs) off = let xWidth = fromIntegral $ imageWidth x
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

encodeIco :: IcoFile -> BL.ByteString
encodeIco (Ico0 h es bs) = runPut $ put h >> (mapM_ put es) >> mapM_ encodeImgDataWithHeader bs
encodeIco (Ico1 bs) = let
                        ih = IcoHeader {
                              icoType = Icon,
                              icoCount = fromIntegral $ length bs
                            }
                        heads = getHeaders bs (toInteger $ sizeofIcoHeader)
                        ies = map fst heads
                        bihs = map snd heads
                      in runPut $ put ih >> mapM_ put ies >> mapM_ put bihs >> mapM_ encodeImgData bs

mencode = encodeIco

--Some test cases
andpixel :: Image Pixel8
andpixel = Image 1 1 (V.singleton 0)

andsquare :: Image Pixel8
andsquare = Image 2 2 (V.fromList [0, 0, 0, 0])

whitepixel :: Image PixelRGBA8
whitepixel = Image 1 1 (V.fromList [255, 255, 255, 255])

blackpixel :: Image PixelRGBA8
blackpixel = Image 1 1 (V.fromList [0, 0, 0, 255])

filepath :: FilePath
filepath = "/home/franco/ej.ico"

square :: Image PixelRGBA8
square = Image 2 2 (V.fromList [0,0,255,255, 0,255,0,255, 255,0,0,255, 255,255,255,255]) --blue, green, red, white

ej10 :: IO ()
ej10 = let ih        = IcoHeader {icoType = Icon, icoCount = 1}
           eh        = [IcoEntry {icoWidth = 1, icoHeight = 1, icoColorCount = 0, icoPlanes  = 1, icoBitCount = 32, icoSize = 48, icoOffset = 22} ]
           bh        = BmpInfoHeader {size = 40, width = 1, height = 2, planes = 1, bitPerPixel = 32, bitmapCompression = 0, byteImageSize = 4, xResolution = 0, yResolution = 0, colorCount = 0, importantColours = 0}
           bm        = [(bh, blackpixel, andpixel)]
           ico       = Ico0 ih eh bm
           encoded   = mencode ico
       in BL.writeFile filepath encoded

ej11 :: IO ()
ej11 = let ico = Ico1 [(whitepixel, andpixel)]
           encoded = mencode ico
       in BL.writeFile "/home/franco/ej2.ico" encoded

ej21 :: IO ()
ej21 = let ih        = IcoHeader {icoType = Icon, icoCount = 1}
           eh        = [IcoEntry {icoWidth = 2, icoHeight = 2, icoColorCount = 0, icoPlanes  = 1, icoBitCount = 32, icoSize = 64, icoOffset = 22} ]
           bh        = BmpInfoHeader {size = 40, width = 2, height = 4, planes = 1, bitPerPixel = 32, bitmapCompression = 0, byteImageSize = 16, xResolution = 0, yResolution = 0, colorCount = 0, importantColours = 0}
           bm        = [(bh, square, andsquare)]
           ico       = Ico0 ih eh bm
           encoded   = mencode ico
       in BL.writeFile "/home/franco/sq.ico" encoded

ej22 :: IO ()
ej22 = let ico = Ico1 [(square, andsquare)]
           encoded = mencode ico
       in BL.writeFile "/home/franco/sq2.ico" encoded
