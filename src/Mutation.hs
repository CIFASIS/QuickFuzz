{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns#-}
{-# LANGUAGE FlexibleInstances,UndecidableInstances#-}

module Mutation where

import Test.QuickCheck

import Test.QuickCheck.Gen
import Test.QuickCheck.Gen.Unsafe

-- | Mutation Class
class  Mutation a where
    --mutt' :: Int -> a -> Gen a
    mutt :: a -> Gen a -- ^ Given a value, mutate it.
    --mutt = mutt' 10
    --mut :: Gen a

instance {-#OVERLAPS#-} Mutation a => Mutation [a] where
    mutt x = frequency $ [(20,mapM mutt x), (1, return x)]

instance {-#OVERLAPS#-} Arbitrary a => Mutation a where
    mutt a = frequency $ [ (5, return a), (1,arbitrary)]
