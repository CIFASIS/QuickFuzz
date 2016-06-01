{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}
module BadBmp where

import Images
import Test.QuickCheck

import Data.Binary( Binary(..), encode )
import Data.Binary.Put( runPut )
import Codec.Picture.Bitmap
import Codec.Picture.Types
import Codec.Picture.Metadata

import qualified Data.ByteString.Lazy as L
import qualified Data.Vector.Storable as V

import Data.DeriveTH
import DeriveArbitrary

import Megadeth.DeriveShow

-- type BmpFile  = (BmpPalette, Image PixelRGBA8)
type BmpFile  = (BmpHeader, BmpInfoHeader, BmpPalette, Image PixelRGBA8)

--derive makeArbitrary ''BmpPalette
--derive makeArbitrary ''BmpInfoHeader
--derive makeArbitrary ''BmpHeader

--derive makeShow ''BmpPalette
--derive makeShow ''BmpHeader

$(devArbitrary ''BmpFile)
$(devShow ''BmpFile)

{-
goodMagicId :: Gen BmpFile
goodMagicId = do
                h   <- arbitrary :: Gen BmpHeader
                ih  <- arbitrary :: Gen BmpInfoHeader
                pal <- arbitrary :: Gen BmpPalette
                bm  <- arbitrary :: Gen Image PixelRGBA8
                return $ (h {magicIdentifier = bitmapMagicIdentifier}, ih, pal, bm)
-}

encodeBMPFile :: BmpFile -> L.ByteString
encodeBMPFile (hdr, info, pal, img) = runPut $ put hdr >> put info >> putPalette pal >> bmpEncode img
--encodeBMPFile (pal,img) = encodeBitmapWithPalette pal img

mencode = encodeBMPFile

square :: Image PixelRGBA8
square = Image 2 2 (V.fromList [0,0,255,255, 0,255,0,255, 255,0,0,255, 255,255,255,255]) --blue, green, red, white

ej :: IO ()
ej = let head      = BmpHeader {magicIdentifier = bitmapMagicIdentifier, fileSize = 70, reserved1 = 0, reserved2 = 0, dataOffset = 54}
         bih       = BmpInfoHeader {size = 40, width = 2, height = 2, planes = 1, bitPerPixel = 32, bitmapCompression = 0, byteImageSize = 16, xResolution = 0, yResolution = 0, colorCount = 0, importantColours = 0}
         pal       = BmpPalette []
         bm        = square
         bmp       = (head, bih, pal, bm)
         encoded   = mencode bmp
     in L.writeFile "/home/franco/sq.bmp" encoded
