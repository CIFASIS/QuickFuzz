{-# LANGUAGE TemplateHaskell #-}
module DeriveEq where

import           Data.Derive.Eq
import           Data.DeriveTH
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Megadeth.Prim

isArbInsName = isinsName ''Eq

devEq :: Name -> Q [Dec]
devEq = megaderive (derive makeEq) isArbInsName
