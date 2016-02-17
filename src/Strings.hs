module Strings where

import Test.QuickCheck
import Data.Char (chr)


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

