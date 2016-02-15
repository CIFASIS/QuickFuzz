{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Tar where

import Test.QuickCheck
import DeriveArbitrary

import Codec.Archive.Tar.Entry
import Codec.Archive.Tar.Types
import Codec.Archive.Tar.Write

import System.Posix.Types

import qualified Data.ByteString.Lazy as L
import Data.DeriveTH
import Data.Word(Word32)

import Vector
import ByteString

$(showDeps ''Entry)

mencode ::  [Entry] -> L.ByteString
mencode = write
