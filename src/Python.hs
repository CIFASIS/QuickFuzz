{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module Python where

import Test.QuickCheck
import Language.Python.Common
import Language.Python.Common.Pretty

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC8

import DeriveArbitrary
import Strings 

type MPy = Module ()

$(devArbitrary ''MPy)

mencode :: MPy -> LC8.ByteString
mencode x = LC8.pack $ prettyText x

