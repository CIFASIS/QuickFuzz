{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module DeriveArbitrary where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Test.QuickCheck
import Control.Monad
import Control.Arrow
import Control.Applicative
import Data.List

--import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Graph as G
import qualified Data.Map.Strict as M
import Control.Monad.Trans.State.Lazy
import qualified Control.Monad.Trans.Class as TC

import DeriveArbitrary

-- | Mutation Class
class Arbitrary a => Mutation a where
    mutt :: a -> a -- ^ Given a value, mutate it.
    mut :: a
    --  mut = unsafePerformIO $ generate $ resize 10 arbitrary
    --  Suck it purity ?

