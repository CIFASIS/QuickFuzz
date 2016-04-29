{-# LANGUAGE FlexibleInstances,UndecidableInstances #-}
module ProbGen where

import Test.QuickCheck

class ProbGen a where
    prob_gen :: [Int] -> Int -> Gen a

-- instance {-#OVERLAPS#-} Arbitrary a => ProbGen a where
--     prob_gen _ _ = arbitrary
--
sized_gen :: (ProbGen a) => [Int] -> Gen a
sized_gen xs = sized $ prob_gen xs

instance ProbGen a => ProbGen [a] where
    prob_gen xs n = 
        do  k <- choose (0,n)
            sequence [prob_gen xs n | _ <- [1..k]]
