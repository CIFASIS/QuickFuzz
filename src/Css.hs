{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module Css where

import Args
import Test.QuickCheck
import Check

import Data.Binary( Binary(..), encode )

import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Char8 as C8
import Data.DeriveTH

import DeriveArbitrary
import ByteString
import Vector

import Text.PrettyPrint

import Language.Css.Syntax
import Language.Css.Pretty
import Language.Css.Build

import Data.Monoid
import Data.List.Split
import Data.Char (chr)
import qualified Data.Text as T

import Linear

type MCssFile  = StyleSheet

genName :: Gen String
genName = listOf1 validChars :: Gen String
  where validChars = chr <$> choose (97, 122)

instance Arbitrary String where
   --arbitrary = genName
   arbitrary = oneof $ Prelude.map return ["a", "b", "c", "d", "e"]

$(showDeps ''MCssFile)

encodeMCssFile x = LC8.pack $ (render (pretty x)) 

mencode :: MCssFile -> LC8.ByteString
mencode = encodeMCssFile

