{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module GLSL where

import Test.QuickCheck
import Language.GLSL.Syntax
import Language.GLSL.Pretty
import Text.PrettyPrint.HughesPJClass

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC8

import DeriveArbitrary
import Strings 

instance Arbitrary String where
   arbitrary = mgenName

type MGLSL = TranslationUnit

$(devArbitrary ''MGLSL)

mencode :: MGLSL -> LC8.ByteString
mencode x = LC8.pack $ render $ option (Just x)

