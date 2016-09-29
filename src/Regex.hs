{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module Regex where

import Test.QuickCheck

--import Text.XML.Light.Output( ppcTopElement, prettyConfigPP )
--import Text.XML.Light.Types

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC8

import Data.DeriveTH
import DeriveArbitrary

import Strings
import ByteString

import Data.Char (chr)

data Regex = Sym String
           | InRange String
           | NotInRange String
           | Alt Regex Regex
           | Seq Regex Regex
           | Rep Regex
           | Plus Regex
           | OrNothing Regex
           -- | Tincho [Pep]

--genchar = oneof $ map return ['a', 'b', 'c']

instance {-# OVERLAPPING #-} Arbitrary String where
   arbitrary = mgenName

$(devArbitrary ''Regex)

instance Show Regex where
  show (Sym s)  =  s
  show (Alt p q)  =  "(" ++ (show p) ++ "|" ++ (show q) ++ ")"
  show (Seq p q) = (show p) ++ (show q)
  show (Rep r)    =  "(" ++ (show  r) ++ ")*"
  show (Plus r)    =  "(" ++ (show  r) ++ ")+"
  show (OrNothing r)    =  "(" ++ (show  r) ++ ")?"
  show (InRange r)    =  "[" ++ (r :: String) ++ "]"
  show (NotInRange r)    =  "[^" ++ (r :: String) ++ "]"

type MRegex = Regex

mencode :: MRegex -> LC8.ByteString
mencode x = LC8.pack $ "/" ++ show x ++ "/"
