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

instance (ProbGen a, ProbGen b) => ProbGen (a,b) where
    prob_gen xs n = (,) <$> prob_gen xs n <*> prob_gen xs n

instance (ProbGen a, ProbGen b, ProbGen c) => ProbGen (a,b,c) where
    prob_gen xs n = (,,) <$> prob_gen xs n <*> prob_gen xs n<*> prob_gen xs n

instance (ProbGen a, ProbGen b, ProbGen c, ProbGen d) => ProbGen (a,b,c,d) where
    prob_gen xs n = (,,,) <$> prob_gen xs n <*> prob_gen xs n<*> prob_gen xs n<*> prob_gen xs n

instance (ProbGen a,
         ProbGen b, ProbGen c,
         ProbGen d,
         ProbGen e
        ) => ProbGen (a,b,c,d,e) where
    prob_gen xs n = (,,,,) <$> prob_gen xs n <*> prob_gen xs n<*> prob_gen xs n
                <*> prob_gen xs n
                <*> prob_gen xs n

instance (ProbGen a,
         ProbGen b, ProbGen c,
         ProbGen d,
         ProbGen e,
         ProbGen f
        ) => ProbGen (a,b,c,d,e,f) where
    prob_gen xs n = (,,,,,) <$> prob_gen xs n <*> prob_gen xs n<*> prob_gen xs n
                <*> prob_gen xs n
                <*> prob_gen xs n
                <*> prob_gen xs n

instance ProbGen Int where
    prob_gen [] n = choose (0,n)
    prob_gen (x:xs) n = choose (0, min x n)

instance ProbGen Bool where 
    prob_gen (x:y:ys) _ = frequency [(x,return True), (y,return False)]
    prob_gen _ _ = arbitrary

