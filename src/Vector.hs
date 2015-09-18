{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module Vector where

import Test.QuickCheck
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
import Data.Word(Word8)

instance Arbitrary a => Arbitrary (V.Vector a) where
   arbitrary = do 
     l <- listOf arbitrary
     return $ V.fromList l

instance (VU.Unbox a, Arbitrary a) => Arbitrary (VU.Vector a) where
   arbitrary = do 
     l <- listOf arbitrary
     return $ VU.fromList l

instance (VS.Storable a, Arbitrary a) => Arbitrary (VS.Vector a) where
   arbitrary = do 
     l <- listOf arbitrary
     return $ VS.fromList l

instance Arbitrary (V.Vector (VU.Vector Word8)) where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Word8)
     x <- (arbitrary :: Gen Int)
     --return $ V.replicate (x `mod` 32) (VU.fromList l)
     return $ V.replicate x (VU.fromList l)



