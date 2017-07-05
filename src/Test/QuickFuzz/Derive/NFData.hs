{-# LANGUAGE TemplateHaskell #-}

module Test.QuickFuzz.Derive.NFData where

import Control.DeepSeq
import Data.Derive.NFData
import Data.DeriveTH

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Megadeth.Prim

isArbInsName = isinsName ''NFData

devNFData :: Name -> Q [Dec]
devNFData = megaderive (derive makeNFData) isArbInsName
