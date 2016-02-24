{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns#-}
{-# LANGUAGE FlexibleInstances,UndecidableInstances#-}

module Mutation where

import Test.QuickCheck

-- | Mutation Class
class  Mutation a where
    mutt :: a -> Gen a -- ^ Given a value, mutate it.
    mut :: Gen a

