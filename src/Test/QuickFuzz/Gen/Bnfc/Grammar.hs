{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE DeriveGeneric #-}


module Test.QuickFuzz.Gen.Bnfc.Grammar where

import Test.QuickFuzz.Gen.Bnfc.PrintGrammar as Print
import Test.QuickFuzz.Gen.Bnfc.AbsGrammar as Abstract

import Data.Char
import Data.Default
import Data.Text.Encoding (encodeUtf8)
import Control.DeepSeq
import Test.QuickCheck
import Test.QuickFuzz.Derive.Arbitrary
import Test.QuickFuzz.Derive.Show
import Test.QuickFuzz.Derive.NFData
--import Test.QuickFuzz.Derive.Mutation
import Test.QuickFuzz.Gen.FormatInfo
import Test.QuickFuzz.Gen.Base.ByteString
import qualified Data.ByteString.Lazy.Char8 as L8

devShow ''Abstract.Exp
devArbitrary ''Abstract.Exp
devNFData ''Abstract.Exp
--devMutation ''Abstract.Exp


bnfcInfo :: FormatInfo Exp NoActions
bnfcInfo = def
  { encode  =  L8.pack . Print.printTree
  , random = arbitrary
  , value = show
  , ext = "ext"
  }

-- ./install_fuzzers.sh ; rm -rf outdir ; stack install --flag QuickFuzz:all ; QuickFuzz gen bnfc
