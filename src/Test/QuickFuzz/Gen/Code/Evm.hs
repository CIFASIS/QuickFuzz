{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.QuickFuzz.Gen.Code.Evm (evmInfo) where

import Data.Default
import Prelude hiding (GT,LT,EQ)

import Test.QuickCheck
import Control.DeepSeq
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.List

import Data.ByteString as BS
import Data.ByteString.Lazy as L

import Blockchain.Data.Code
import Blockchain.VM.Code
import Blockchain.VM.Opcodes
import Blockchain.Util
import Blockchain.ExtWord

import Test.QuickFuzz.Derive.Arbitrary
import Test.QuickFuzz.Derive.Fixable
import Test.QuickFuzz.Derive.Show
import Test.QuickFuzz.Derive.NFData
import Test.QuickFuzz.Derive.Mutation
import Test.QuickFuzz.Gen.FormatInfo
import Test.QuickFuzz.Gen.Base.ByteString
import Test.QuickFuzz.Gen.Base.String

type EvmBytecode = [Operation]
 
devArbitrary ''EvmBytecode
devShow ''EvmBytecode
devNFData ''EvmBytecode

disasmBSAt :: BS.ByteString -> Word256 -> Int -> [Operation]
disasmBSAt "" _ _ = []
disasmBSAt _ _ 0 = []
disasmBSAt bs base limit =
  op : disasmBSAt (safeDrop next bs) (base + next) (limit - 1)
  where
    (op, next) = getOperationAt' bs 0

decode' x = disasmBSAt (L.toStrict x) 0 10240

evmInfo :: FormatInfo EvmBytecode NoActions
evmInfo = def 
    { encode = L.fromStrict . codeBytes . compile
    , decode = decode'
    , mutate = mutt
    , random = arbitrary
    , value = show
    , ext = "evm" 
    } 
