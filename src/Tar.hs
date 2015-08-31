{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Tar where

import Test.QuickCheck
import Check
import DeriveArbitrary


--import Control.Monad.Zip
--import Control.Exception
import Data.Binary( Binary(..), encode )

import Codec.Archive.Tar.Entry
import Codec.Archive.Tar.Types
import Codec.Archive.Tar.Write

import System.Posix.Types

import qualified Data.ByteString.Lazy as L
import Data.DeriveTH
import Data.Word(Word8, Word16, Word32)
--import Data.Int( Int16, Int8 )

import Vector
import ByteString

-- $(deriveArbitraryRec ''Entry) (not working)

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

mencode ::  [Entry] -> L.ByteString
mencode = write

instance Arbitrary Permissions where
   arbitrary = do
     w32 <- arbitrary :: Gen Word32
     return $ CMode w32

main = quickCheckWith stdArgs { maxSuccess = 12000000, maxSize = 500 } (checkprop "buggy_qc.tar" "/usr/bin/bsdtar" ["-tvf", "buggy_qc.tar"] mencode)
