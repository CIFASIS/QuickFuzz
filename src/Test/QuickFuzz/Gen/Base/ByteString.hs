{-# LANGUAGE FlexibleInstances, IncoherentInstances#-}
module Test.QuickFuzz.Gen.Base.ByteString where

import Test.QuickCheck

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

import Data.Word(Word8, Word16, Word32)
import Data.Int( Int16, Int8 )
import Data.List( init )

import Test.QuickFuzz.Gen.Base.Value

instance Arbitrary B.ByteString where
   arbitrary = do 
     --l <- listOf (arbitrary :: Gen Word8)
     x <- genStrValue "ByteString"
     return $ L8.toStrict $ L8.pack x

instance Arbitrary L.ByteString where
   arbitrary = do
     --l <- listOf (arbitrary :: Gen Word8)
     x <- genStrValue "ByteString"
     return $ L8.pack x

   shrink xs = ys -- ++ concat (map shrink ys)
                where ys = tail (L.tails xs) ++ init (L.inits xs)

instance CoArbitrary L.ByteString where
   coarbitrary x = coarbitrary $ L.unpack x
