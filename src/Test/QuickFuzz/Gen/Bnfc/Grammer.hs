{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE DeriveGeneric #-}


module Test.QuickFuzz.Gen.Bnfc.Grammer
( module LexGrammer,
  module ParGrammer,
  module SkelGrammer,
  module PrintGrammer,
  module AbsGrammer,
  module ErrM
) where

import LexGrammer
import ParGrammer
import SkelGrammer
import PrintGrammer
import AbsGrammer
import ErrM

import Data.Char
import Data.Default
import Data.Text.Encoding (encodeUtf8)


import Control.DeepSeq
import Test.QuickCheck

import Test.QuickFuzz.Derive.Arbitrary
import Test.QuickFuzz.Derive.Show
import Test.QuickFuzz.Derive.NFData
import Test.QuickFuzz.Gen.FormatInfo
import Test.QuickFuzz.Gen.Base.ByteString
import qualified Data.ByteString.Lazy.Char8 as L8


-- stack install --flag QuickFuzz:all
-- stack install --flag QuickFuzz:image
-- QuickFuzz gentest img "python3 test.py @@ " -l 1 -u 10 -f radamsa

bnfcInfo :: FormatInfo Exp NoActions
bnfcInfo = def
  { encode  =  L8.pack . printTree
  , random = arbitrary
  , value = show
  , ext = "unknown"
  }


