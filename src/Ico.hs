{-# LANGUAGE TemplateHaskell, FlexibleInstances, DeriveAnyClass, OverloadedStrings #-}

module Ico where

import Images
import Test.QuickCheck

import Data.Binary( Binary(..), encode )
import Data.Binary.Put(putWord8, putWord16le, putWord32le)
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
  , icoEReserved  :: !Word8
  , icoPlanes     :: !Word16
  , icoBitCount   :: !Word16
  , icoSize       :: !Word32
  , icoOffset     :: !Word32
  } deriving (Show, Read, Eq)

data IcoFile = IcoFile IcoHeader [IcoEntry] [(Image PixelRGBA8, Image PixelRGBA8)] deriving Show--(xor, and); and is 1bit (different type?)

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
                  putWord8 $ icoEReserved entry
                  putWord16le $ icoPlanes entry
                  putWord16le $ icoBitCount entry
                  putWord32le $ icoSize entry
                  putWord32le $ icoOffset entry
  get = undefined

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


encodeMasks :: [(Image PixelRGBA8, Image PixelRGBA8)] -> BL.ByteString
encodeMasks [] = BL.empty
encodeMasks ((x,a):xs) = let ex = encodeBitmap x
                             ea = encodeBitmap a
                         in BL.append (BL.append ex ea) (encodeMasks xs)

encodeIco :: IcoFile -> BL.ByteString
encodeIco (IcoFile h es bs) = BL.append (encode h) (BL.append (encode es) (encodeMasks bs))

mencode = encodeIco

cons4 :: V.Storable a => a -> a -> a -> a -> V.Vector a
cons4 r g b a = V.cons r (V.cons g (V.cons b (V.singleton a)))

andmask :: Image PixelRGBA8
andmask = Image 1 1 (cons4 1 1 1 1)

whitepixel :: Image PixelRGBA8
whitepixel = Image 1 1 (cons4 255 255 255 0)

blackpixel :: Image PixelRGBA8
blackpixel = Image 1 1 (cons4 0 0 0 255)

filepath :: FilePath
filepath = "/home/franco/ej.ico"

ej1 :: IO ()
ej1 = let ih        = IcoHeader {icoType = Icon, icoCount = 1}
          eh        = [IcoEntry {icoWidth = 1, icoHeight = 1, icoColorCount = 0, icoEReserved = 0, icoPlanes  = 0, icoBitCount = 32, icoSize = 1, icoOffset = 22} ]
          bm        = [(andmask, whitepixel)]
          ico       = IcoFile ih eh bm
          encoded   = mencode ico
      in BL.writeFile filepath encoded
