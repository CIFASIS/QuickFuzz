module Strings where

import Test.QuickCheck
import Data.Char (chr)

import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL

-- Text
instance Arbitrary TS.Text where
    arbitrary = TS.pack <$> arbitrary
    shrink xs = TS.pack <$> shrink (TS.unpack xs)

instance Arbitrary TL.Text where
    arbitrary = TL.pack <$> arbitrary
    shrink xs = TL.pack <$> shrink (TL.unpack xs)

instance CoArbitrary TS.Text where
    coarbitrary = coarbitrary . TS.unpack

instance CoArbitrary TL.Text where
    coarbitrary = coarbitrary . TL.unpack

genName :: Gen String
genName = listOf1 validChars :: Gen String
  where validChars = chr <$> choose (97, 122)

sgenName :: Int -> Gen String
sgenName 1 = do
        c <- chr <$> choose (97,122)
        return $ [c]
sgenName n = do
        c <- chr <$> choose (97,122)
        n <- sgenName (max (n `div` 2) 1)
        return $ c : n

mgenName = oneof $ map return ["a", "b", "c"]

