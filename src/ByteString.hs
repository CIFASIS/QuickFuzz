{-# LANGUAGE FlexibleInstances, IncoherentInstances#-}
module ByteString where

import Test.QuickCheck

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

import Data.Word(Word8, Word16, Word32)
import Data.Int( Int16, Int8 )
import Data.List( init )

instance Arbitrary B.ByteString where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Word8)
     return $ B.pack l

instance Arbitrary L.ByteString where
   arbitrary = do
     l <- listOf (arbitrary :: Gen Word8)
     return $ L.pack l

   shrink xs = ys -- ++ concat (map shrink ys)
                where ys = tail (L.tails xs) ++ init (L.inits xs)

instance CoArbitrary L.ByteString where
   coarbitrary x = coarbitrary $ L.unpack x

bencode = id
