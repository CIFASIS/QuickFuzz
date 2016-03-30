{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module Dot where

import Test.QuickCheck 
import Data.Binary( Binary(..), encode )
import Data.DeriveTH

import DeriveArbitrary
import ByteString
import Vector

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

import Language.Dot.Syntax
import Language.Dot.Pretty

import Data.Char (chr)
--import Data.List.Split

genName :: Gen String
genName = listOf1 validChars :: Gen String
  where validChars = chr <$> choose (97, 122)

instance {-# OVERLAPPING #-} Arbitrary String where
   arbitrary = genName

$(devArbitrary ''Graph)

mencode :: Graph -> L8.ByteString
mencode = L8.pack . renderDot 
