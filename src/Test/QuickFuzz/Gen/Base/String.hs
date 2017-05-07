{-# LANGUAGE FlexibleInstances, IncoherentInstances #-}
module Test.QuickFuzz.Gen.Base.String where

import Test.QuickCheck

import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Test.QuickFuzz.Gen.Base.Value

-- String

instance Arbitrary String where
    arbitrary = genStrValue "String"

-- Text
instance Arbitrary TS.Text where
    arbitrary = TS.pack <$> genStrValue "Text"
    shrink xs = TS.pack <$> shrink (TS.unpack xs)

instance Arbitrary TL.Text where
    arbitrary = TL.pack <$> genStrValue "Text"
    shrink xs = TL.pack <$> shrink (TL.unpack xs)

instance CoArbitrary TS.Text where
    coarbitrary = coarbitrary . TS.unpack

instance CoArbitrary TL.Text where
    coarbitrary = coarbitrary . TL.unpack
