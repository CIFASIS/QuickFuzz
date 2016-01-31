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

instance Arbitrary Permissions where
   arbitrary = do
     w32 <- arbitrary :: Gen Word32
     return $ CMode w32

derive makeArbitrary ''LinkTarget
$(deriveArbitraryRec ''Entry)

derive makeShow ''Entry
derive makeShow ''EntryContent
derive makeShow ''TarPath
derive makeShow ''LinkTarget
derive makeShow ''Format
derive makeShow ''Ownership

mencode ::  [Entry] -> L.ByteString
mencode = write
