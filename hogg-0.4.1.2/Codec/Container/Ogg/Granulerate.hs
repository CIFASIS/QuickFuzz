--
-- Module      : Granulerate
-- Copyright   : (c) Conrad Parker 2006
-- License     : BSD-style
-- Maintainer  : conradp@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable

module Codec.Container.Ogg.Granulerate (
  Granulerate (..),
  intRate,
  fracRate
) where

import Data.Ratio

------------------------------------------------------------
-- Types
--

newtype Granulerate = Granulerate Rational
  deriving Eq

------------------------------------------------------------
-- Granulerate functions
--

intRate :: Integer -> Granulerate
intRate x = Granulerate (x % 1)

fracRate :: Integer -> Integer -> Granulerate
fracRate n d = Granulerate (n % d)

instance Show Granulerate where
  show (Granulerate r)
    | d == 1    = show n
    | otherwise = show n ++ "/" ++ show d
    where n = numerator r
          d = denominator r
