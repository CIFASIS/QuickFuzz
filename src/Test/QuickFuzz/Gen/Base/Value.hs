{-# LANGUAGE FlexibleInstances, IncoherentInstances #-}
module Test.QuickFuzz.Gen.Base.Value where

import Test.QuickCheck
import Data.Char (chr)

import Test.QuickFuzz.Global

genStrValue :: String -> Gen String
genStrValue name = sized $ \n -> do
            n <- arbitrary :: Gen Int
            f <- arbitrary :: Gen Float
            x <- arbitrary :: Gen Char
            --s <- resize (max n 10) genName :: Gen String
            ss <- mgenName :: Gen String 
            --ls <- lgenName :: Gen String
            frequency $ zip freqs $ map return [ show n, show f, [x], ss]
            where freqs = getFreqs name

genName :: Gen String
genName = listOf1 validChars :: Gen String
  where validChars = chr <$> choose (97, 122)

lgenName :: Gen String
lgenName =  do
            n <- arbitrary :: Gen Int
            x <- arbitrary :: Gen Char
            return $ replicate (1000 * (n `mod` 50)) x

sgenName :: Int -> Gen String
sgenName 1 = do
        c <- chr <$> choose (97,122)
        return $ [c]

sgenName n = do
        c <- chr <$> choose (97,122)
        n <- sgenName (max (n `div` 2) 1)
        return $ c : n

mgenName = oneof $ map return ["a", "b", "c"]
