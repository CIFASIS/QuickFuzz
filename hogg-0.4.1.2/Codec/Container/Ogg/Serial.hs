{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Module      : Serial
-- Copyright   : (c) Conrad Parker 2007
-- License     : BSD-style
-- Maintainer  : conradp@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable

module Codec.Container.Ogg.Serial (
  Serial,
  Serialled,
  serialOf,
  demux,
  genSerial
) where

import Data.Word (Word32)
import System.Random
import Codec.Container.Ogg.List

type Serial = Word32

------------------------------------------------------------
-- | Typeclass: Serialled
--

class Serialled a where
  serialOf :: a -> Serial

serialEq :: (Serialled a, Serialled b) => a -> b -> Bool
serialEq a b = (serialOf a) == (serialOf b)

------------------------------------------------------------
-- | Demux
--

demux :: (Serialled a) => [a] -> [[a]]
demux = classify serialEq

------------------------------------------------------------
-- | Generate a serial number
--

genSerial :: IO Serial
genSerial = do
  -- Randomly generate a Serial that does not equal
  -- 0xffffffff, as this value is treated specailly by libogg
  serialno <- getStdRandom (randomR (0,0xffffffff-1))
  return serialno
