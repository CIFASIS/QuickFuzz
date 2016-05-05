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
