{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module Xml where

import Test.QuickCheck
import Text.XML.HaXml.Types
import Text.XML.HaXml.ByteStringPP

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC8

import DeriveArbitrary
import DeriveMutation
import Strings 

instance Arbitrary EncodingDecl where
   arbitrary = oneof $ map (return . EncodingDecl) ["UTF-8"]
 
instance Arbitrary XMLDecl where
   arbitrary = do 
                ver <- oneof $ map return ["1.0"]
                (x,y) <- arbitrary
                return $ XMLDecl ver x y

instance Arbitrary String where
   arbitrary = mgenName
   --arbitrary = sized sgenName 

type MXml = Document ()

$(devArbitrary ''MXml)
$(devMutationRec ''MXml)

mencode :: MXml -> LC8.ByteString
mencode x = document x

