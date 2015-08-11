--
-- Module      : Granulepos
-- Copyright   : (c) Conrad Parker 2006
-- License     : BSD-style
-- Maintainer  : conradp@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable

module Codec.Container.Ogg.Granulepos (
  Granulepos (..),
  gpPack,
  gpUnpack
) where

import Data.Word (Word64)

------------------------------------------------------------
-- Types
--

newtype Granulepos = Granulepos (Maybe Word64)
  deriving Eq

------------------------------------------------------------
-- Granulepos functions
--

gpPack :: Word64 -> Granulepos
gpPack 0xffffffffffffffff = Granulepos Nothing
gpPack gp = Granulepos (Just gp)

gpUnpack :: Granulepos -> Word64
gpUnpack (Granulepos (Nothing)) = -1
gpUnpack (Granulepos (Just gp)) = gp

instance Show Granulepos where
  show (Granulepos (Nothing)) = "-1"
  show (Granulepos (Just gp)) = show gp
