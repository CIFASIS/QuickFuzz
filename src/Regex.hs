{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module Regex where

import Test.QuickCheck

--import Text.XML.Light.Output( ppcTopElement, prettyConfigPP )
--import Text.XML.Light.Types

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC8

import Data.DeriveTH
import DeriveArbitrary

import Vector
import ByteString

import Data.Char (chr)

data Regex = Sym Char
           | InRange [Char]
           | NotInRange [Char]
           | Alt Regex Regex
           | Seq Regex Regex
           | Rep Regex
           | Plus Regex
           | OrNothing Regex

genchar = oneof $ map return ['a', 'b', 'c']

instance Arbitrary Regex where
    arbitrary = sized mk_regex

mk_regex :: Int -> Gen Regex
mk_regex 0     = do
                  x <- genchar
                  xs <- listOf1 $ genchar
                  oneof $ map return [Sym x, InRange xs, NotInRange xs]

mk_regex n     = do
                     s <- (mk_regex 0)
                     r1 <- (mk_regex (n-1))
                     r2 <- (mk_regex (n-1))
                     frequency $ map (\(x,y) -> (x, return y)) [(1,s), (10,Alt r1 r2), (10,Seq r1 r2), (10,Rep r1), (10, Plus r1), (10, OrNothing r1)]

instance Show Regex where
  show (Sym s)  =  [s]
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
