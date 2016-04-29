{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module Xml where

import Test.QuickCheck
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Types
import Text.XML.HaXml.ByteStringPP

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Char8 as C8
import Data.Maybe

import Mutation
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


instance Arbitrary [Misc] where
   --arbitrary = genName
   arbitrary = do
                 (a1, a2, a3) <- arbitrary
                 oneof $ map return [[], [a1], [a2], [a3]]


instance (Arbitrary i) => Arbitrary [Content i] where
   --arbitrary = genName
   arbitrary = do
                 (a1, a2, a3) <- arbitrary
                 oneof $ map return [[], [a1], [a2], [a3]]


instance Arbitrary String where
   --arbitrary = genName
   arbitrary = mgenName 

data MXml = MXml (Document Posn) deriving Show

$(devArbitrary ''MXml)
-- $(createIntGen ''MXml)
$(devIntGen ''MXml)
-- $(devMutationRec ''MXml)

mgen :: [Int] -> Gen MXml
mgen = customGen_Xml_MXml

readFiles :: [FilePath] -> [IO LC8.ByteString]
readFiles = map LC8.readFile

mencode :: MXml -> LC8.ByteString
mencode (MXml x)= Text.XML.HaXml.ByteStringPP.document x


{-
mencode x = unsafePerformIO ( 
             do r <- timeout 10000 $ evaluate $ Text.XML.HaXml.ByteStringPP.document x
                case r of
                  Just x -> return x --unsafePerformIO $ return x
                  Nothing -> return $ LC8.pack "" --unsafePerformIO $ return $ LC8.pack ""
             )
-}
--mhandler1 :: SomeException -> IO (Maybe (Document Posn))
--mhandler1 x = return $ Nothing
 
--mdecode :: C8.ByteString -> (Document Posn)
--mdecode xml =  xmlParse "" (C8.unpack xml) 
              -- unsafePerformIO $ catch ( evaluate $ Just $ xmlParse "" (C8.unpack xml)) mhandler1
