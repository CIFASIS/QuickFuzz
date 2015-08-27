{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module Xml where

import Test.QuickCheck
import Check

import Control.Monad.Zip
import Control.Exception
import Data.Binary( Binary(..), encode )

import Text.XML.HaXml.Types
import Text.XML.HaXml.ByteStringPP

import qualified Data.ByteString.Lazy as L

import Data.DeriveTH
import DeriveArbitrary

import Vector
import ByteString 

derive makeArbitrary ''Document
derive makeArbitrary ''Misc
derive makeArbitrary ''Element
derive makeArbitrary ''EntityDef
derive makeArbitrary ''Content
derive makeArbitrary ''Prolog
derive makeArbitrary ''AttValue
derive makeArbitrary ''NDataDecl
derive makeArbitrary ''Reference
derive makeArbitrary ''ExternalID
derive makeArbitrary ''QName
derive makeArbitrary ''DocTypeDecl
derive makeArbitrary ''EntityValue
derive makeArbitrary ''XMLDecl
derive makeArbitrary ''MarkupDecl
derive makeArbitrary ''Namespace
derive makeArbitrary ''PubidLiteral
derive makeArbitrary ''EV
derive makeArbitrary ''NotationDecl
derive makeArbitrary ''EncodingDecl
derive makeArbitrary ''SystemLiteral
derive makeArbitrary ''EntityDecl
derive makeArbitrary ''PublicID
derive makeArbitrary ''AttListDecl
derive makeArbitrary ''PEDecl


derive makeArbitrary ''ElementDecl
derive makeArbitrary ''PEDef
derive makeArbitrary ''AttDef
derive makeArbitrary ''GEDecl
derive makeArbitrary ''ContentSpec
derive makeArbitrary ''DefaultDecl
derive makeArbitrary ''AttType
derive makeArbitrary ''CP
derive makeArbitrary ''FIXED

derive makeArbitrary ''Mixed
derive makeArbitrary ''EnumeratedType
derive makeArbitrary ''Modifier
derive makeArbitrary ''TokenizedType

type MXml  = Document () 

mencode :: MXml -> L.ByteString
mencode x = document x

main = quickCheckWith stdArgs { maxSuccess = 12000, maxSize = 9 } (fuzzprop "buggy_qc.xml" "/usr/bin/xmllint" ["html", "buggy_qc.xml"] mencode)
--main = quickCheckWith stdArgs { maxSuccess = 1200, maxSize = 50 } (genprop "buggy_qc.jp2" "" [] mencode "data/xml")
