{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Pnm where

import Args
import Test.QuickCheck
import Check

import qualified Codec.PPM.Binary as BPPM
import qualified Codec.PPM.Text as TPPM

import Data.Binary( Binary(..), encode )
import Data.Word(Word8, Word16, Word32)

import qualified Data.ByteString.Lazy as L
import DeriveArbitrary

import Data.List.Split

data MPnmType  = BinaryPnm | TextPnm

instance Show MPnmType where
   show BinaryPnm = "BPPM"
   show TextPnm = "TPPM"

$(deriveArbitraryRec ''MPnmType)

type MPnmImage  = (MPnmType,Integer,Integer, [(Word8,Word8,Word8)])

encodePnmImage (BinaryPnm, x, y, d) = BPPM.stringPPM (x,y) d
encodePnmImage (TextPnm, x, y, d) = BPPM.stringPPM (x,y) d

mencode :: MPnmImage -> L.ByteString
mencode = encodePnmImage

pnmmain (MainArgs _ cmd filename prop maxSuccess maxSize outdir _) = let (prog, args) = (head spl, tail spl) in
    (case prop of
        "zzuf" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ zzufprop filename prog args mencode outdir)
        "check" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ checkprop filename prog args mencode outdir)
        "gen" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ genprop filename prog args mencode outdir)
        "exec" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ execprop filename prog args mencode outdir)
        _     -> error "Invalid action selected"
    ) where spl = splitOn " " cmd

main fargs False = pnmmain $ fargs ""
main fargs True  = processPar fargs pnmmain
