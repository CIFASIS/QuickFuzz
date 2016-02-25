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
--import qualified Data.Text as T

import Mutation
import DeriveMutation

import Strings
import Linear

type MCssFile  = StyleSheet

instance Arbitrary String where
   arbitrary = genName --oneof $ Prelude.map return ["a", "b", "c", "d", "e"]

$(devArbitrary ''MCssFile)
-- $(devMutationRec ''MCssFile)

encodeMCssFile x = LC8.pack $ (render (pretty x)) 

mencode :: MCssFile -> LC8.ByteString
mencode = encodeMCssFile

