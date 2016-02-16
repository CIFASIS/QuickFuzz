{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Tar where

import Test.QuickCheck
import DeriveArbitrary

import Codec.Archive.Tar

import System.Posix.Types

import qualified Data.ByteString.Lazy as L
import Data.DeriveTH
import Data.Word(Word32)

import Vector
import ByteString

$(devArbitrary ''Entry)

mencode ::  [Entry] -> L.ByteString
mencode = write
