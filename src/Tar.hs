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

derive makeArbitrary ''Entry
derive makeShow ''Entry

derive makeArbitrary ''EntryContent
derive makeArbitrary ''TarPath
derive makeShow ''EntryContent
derive makeShow ''TarPath
derive makeArbitrary ''LinkTarget
derive makeShow ''LinkTarget
derive makeArbitrary ''Format
derive makeArbitrary ''Ownership
derive makeShow ''Format
derive makeShow ''Ownership

instance Arbitrary Permissions where
   arbitrary = do
     w32 <- arbitrary :: Gen Word32
     return $ CMode w32

mencode ::  [Entry] -> L.ByteString
mencode = write
