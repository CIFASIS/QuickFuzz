{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module Js where

import Text.PrettyPrint.Leijen
import Language.ECMAScript3.PrettyPrint
import Language.ECMAScript3.Syntax
import Language.ECMAScript3.Syntax.Arbitrary

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

type MJs =  JavaScript String

mencode :: MJs -> L.ByteString
mencode x = L8.pack $ show $ prettyPrint x
