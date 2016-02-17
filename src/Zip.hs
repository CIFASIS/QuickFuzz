{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Zip where

import DeriveArbitrary
import Data.Binary( Binary(..), encode )
import Codec.Archive.Zip

import qualified Data.ByteString.Lazy as L
--import Vector
--import ByteString
import Test.QuickCheck


data MArchive = Archive0 Archive | Archive1 [(FilePath, Integer, L.ByteString)] deriving Show

$(devArbitrary ''MArchive)

mencode :: MArchive -> L.ByteString
mencode (Archive0 x) = encode x
mencode (Archive1 xs) = encode $ foldr addEntryToArchive emptyArchive (map (\(x,y,z) -> toEntry x y z) xs) 
