{-# LANGUAGE TemplateHaskell #-}
module TestTH where

import DeriveArbitrary
import Language.Haskell.TH
import Test.QuickCheck


$(deriveArbitrary ''Expr)
$(deriveArbitrary ''Tree)
