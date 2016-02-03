{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module Xml where

import Test.QuickCheck
import Text.XML.HaXml.Types
import Text.XML.HaXml.ByteStringPP
--import Text.XML.Light.Output( ppcTopElement, prettyConfigPP )
-- import Text.XML.Light.Types

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC8

import Data.DeriveTH
import DeriveArbitrary

import Vector
import ByteString

import Data.Char (chr)

genName :: Gen String
genName = listOf1 validChars :: Gen String
  where validChars = chr <$> choose (97, 122)

sgenName :: Int -> Gen String
sgenName 1 = do
        c <- chr <$> choose (97,122)
        return $ [c]
sgenName n = do
        c <- chr <$> choose (97,122)
        n <- sgenName (n-1)
        return $ c : n

instance Arbitrary String where
   arbitrary = oneof $ map return ["a", "b", "defs"]
   --arbitrary = sized sgenName 

type MXml = Prolog

{-
   sizedElement :: Int -> Gen Element
   sizedElement 0 = do
           en <- arbitrary
           ea <- arbitrary
           el <- arbitrary
           return $ Element en ea [] el
   sizedElement 1 = do
           en <- arbitrary
           ea <- arbitrary
           el <- arbitrary
           return $ Element en ea [] el
   sizedElement n = do
           en <- arbitrary
           ea <- arbitrary
           let n' = (n `div` 10)
           ec <- listOf $ resize n' arbitrary --sizedContent n'
           el <- arbitrary
           return $ Element en ea ec el
-}
-- | Element and contet are mutually recursive.
derive makeArbitrary ''EncodingDecl
derive makeArbitrary ''PubidLiteral
derive makeArbitrary ''SystemLiteral
derive makeArbitrary ''PublicID
derive makeArbitrary ''NDataDecl

-- $(showDeps ''EncodingDecl)
$(showDeps ''MXml)
-- $(showDeps ''MXml)

{-
   $(deriveArbitraryRec ''Element)
   
   derive makeArbitrary ''Content
   --instance Arbitrary Element where
       --arbitrary = sized sizedElement
   -- $(deriveArbitraryRec ''Content)
   --
   {-
      sizedContent :: Int -> Gen Content
      sizedContent 0 = do
          tx <- arbitrary
          str <- arbitrary
          oneof $ map return [Text tx, CRef str]
      sizedContent 1 = do
          tx <- arbitrary
          str <- arbitrary
          oneof $ map return [Text tx, CRef str]
      sizedContent n = do
          el <- resize (n-1) arbitrary --sizedElement (n-1)
          tx <- arbitrary
          str <- arbitrary
          oneof $ map return [Elem el, Text tx, CRef str]
          
      instance Arbitrary Content where
         arbitrary = sized sizedContent 
   -}
   
   -- $(deriveArbitraryRec ''CData)
   
   -- | Attr is al right, there is no mention of the other two types.
   $(deriveArbitraryRec ''Attr)
-}   

mencode :: MXml -> LC8.ByteString
mencode x = prolog x  --LC8.pack $ ppcTopElement prettyConfigPP x

