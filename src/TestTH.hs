{-# LANGUAGE TemplateHaskell #-}
module TestTH where

import DeriveArbitrary
import Language.Haskell.TH
import Test.QuickCheck


$(deriveArbitraryRec ''Expr2)
