{-# LANGUAGE TemplateHaskell #-}
module DeriveShow where

import           Data.Derive.Show
import           Data.DeriveTH
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Megadeth.Prim

isArbInsName = isinsName ''Show

devShow :: Name -> Q [Dec]
devShow = megaderive (derive makeShow) isArbInsName

