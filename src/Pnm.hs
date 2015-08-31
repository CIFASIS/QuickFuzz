{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Pnm where

import Test.QuickCheck
import Check

import qualified Codec.PPM.Binary as BPPM
import qualified Codec.PPM.Text as TPPM

import Data.Binary( Binary(..), encode )
import Data.Word(Word8, Word16, Word32)

import qualified Data.ByteString.Lazy as L

--import Data.DeriveTH
import DeriveArbitrary

import Data.List.Split

data MPnmType  = BinaryPnm | TextPnm
--deriving show

instance Show MPnmType where
   show BinaryPnm = "BPPM"
   show TextPnm = "TPPM"

$(deriveArbitraryRec ''MPnmType)

type MPnmImage  = (MPnmType,Integer,Integer, [(Word8,Word8,Word8)])

encodePnmImage (BinaryPnm, x, y, d) = BPPM.stringPPM (x,y) d
encodePnmImage (TextPnm, x, y, d) = BPPM.stringPPM (x,y) d

mencode :: MPnmImage -> L.ByteString
mencode = encodePnmImage

main filename cmd prop maxSuccess maxSize = let (prog, args) = (head spl, tail spl) in
    (case prop of
        "fuzz" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ fuzzprop filename prog args mencode)
        "check" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ checkprop filename prog args mencode)
        "gen" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ genprop filename prog args mencode)
    ) where spl = splitOn " " cmd
