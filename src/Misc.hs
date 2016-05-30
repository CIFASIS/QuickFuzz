module Misc where

import Test.QuickCheck
import Data.Map
import Data.Monoid

instance Arbitrary a => Arbitrary (Last a) where
       arbitrary = do
           ga <- arbitrary
           oneof $ Prelude.map (return . Last) [Nothing, Just ga]

{-
   instance (Arbitrary a, Arbitrary b) => Arbitrary (Map a b) where
      arbitrary = do
        x <- arbitrary
        y <- arbitrary 
        return $ singleton x y
-}

-- Esto está definido en Test.QuickCheck.Arbitrary ??

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e, Arbitrary f)
      => Arbitrary (a,b,c,d,e,f)
 where
  arbitrary = do
     x1 <- arbitrary
     x2 <- arbitrary
     x3 <- arbitrary
     x4 <- arbitrary
     x5 <- arbitrary
     x6 <- arbitrary
     return (x1,x2,x3,x4,x5,x6)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e, Arbitrary f, Arbitrary g)
      => Arbitrary (a,b,c,d,e,f,g)
 where
  arbitrary = do
     x1 <- arbitrary
     x2 <- arbitrary
     x3 <- arbitrary
     x4 <- arbitrary
     x5 <- arbitrary
     x6 <- arbitrary
     x7 <- arbitrary
     return (x1,x2,x3,x4,x5,x6,x7)
