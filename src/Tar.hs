{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Tar where

import Args
import Test.QuickCheck
import Check
import DeriveArbitrary

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

import Data.List.Split

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

tarmain (MainArgs _ cmd filename prop maxSuccess maxSize outdir b) = let (prog, args) = (head spl, tail spl) in
    (case prop of
        "zzuf" -> quickCheckWith stdArgs { chatty = not b, maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ zzufprop filename prog args mencode outdir)
        "check" -> quickCheckWith stdArgs { chatty = not b, maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ checkprop filename prog args mencode outdir)
        "gen" -> quickCheckWith stdArgs { chatty = not b, maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ genprop filename prog args mencode outdir)
        "exec" -> quickCheckWith stdArgs { chatty = not b, maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ execprop filename prog args mencode outdir)
        _     -> error "Invalid action selected"
    ) where spl = splitOn " " cmd

main fargs False = tarmain $ fargs ""
main fargs True  = processPar fargs tarmain
