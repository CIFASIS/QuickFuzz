{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}
module ByteString where

import Test.QuickCheck

--import Check
--import Test.QuickCheck

--import Data.Binary( Binary(..), encode )

import qualified Data.ByteString.Lazy as L

import Data.Word(Word8, Word16, Word32)
import Data.Int( Int16, Int8 )
--import GHC.Types

instance Arbitrary L.ByteString where
   arbitrary = do
     l <- listOf (arbitrary :: Gen Word8)
     return $ L.pack l

instance CoArbitrary L.ByteString where
   coarbitrary x = coarbitrary $ L.unpack x


--main = quickCheckWith stdArgs { maxSuccess = 1200, maxSize = 1000 } (absprop "buggy_qc.zip" "/usr/bin/unzip" ["buggy_qc.zip"] id)
