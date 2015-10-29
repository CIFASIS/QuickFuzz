{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}
module ByteString where

import Args
import Test.QuickCheck
import Check
import Data.List.Split

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

import Data.Word(Word8, Word16, Word32)
import Data.Int( Int16, Int8 )

instance Arbitrary B.ByteString where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Word8)
     return $ B.pack l

instance Arbitrary L.ByteString where
   arbitrary = do
     l <- listOf (arbitrary :: Gen Word8)
     return $ L.pack l

instance CoArbitrary L.ByteString where
   coarbitrary x = coarbitrary $ L.unpack x

bencode = id

bsmain (MainArgs _ cmd filename prop maxSuccess maxSize outdir b) = let (prog, args) = (head spl, tail spl) in
    (case prop of
        "zzuf" -> quickCheckWith stdArgs { chatty = not b, maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ zzufprop filename prog args bencode outdir)
        "check" -> quickCheckWith stdArgs { chatty = not b, maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ checkprop filename prog args bencode outdir)
        "gen" -> quickCheckWith stdArgs { chatty = not b, maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ genprop filename prog args bencode outdir)
        "exec" -> quickCheckWith stdArgs { chatty = not b, maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ execprop filename prog args bencode outdir)
        _     -> error "Invalid action selected"
    ) where spl = splitOn " " cmd

main fargs False = bsmain $ fargs ""
main fargs True  = processPar fargs bsmain
