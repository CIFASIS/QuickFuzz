{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module JSON where

import Test.QuickCheck
import DeriveArbitrary
import Text.JSON.Types
import Text.JSON.Pretty
import Text.PrettyPrint

import qualified Data.ByteString.Lazy.Char8 as LC8

$(devArbitrary ''JSValue)

mencode :: JSValue -> LC8.ByteString
mencode x = LC8.pack $ render $ pp_value x
