{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module Xml where

import Test.QuickCheck
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Types
import Text.XML.HaXml.ByteStringPP
import Text.XML.HaXml.Combinators

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Char8 as C8
import Data.Maybe

import Mutation
import DeriveArbitrary
import DeriveMArbitrary
import DeriveMutation
import Strings 

$(devActions ["Text.XML.HaXml.Combinators"] ''CFilter False [''Int])
$(devArbitrary ''CFilterAction)
-- $(devArbitraryWithActions False ''CFilter) -- Does not work because of the type var i

instance Arbitrary (CFilter i) where
    arbitrary = do
        x <- arbitrary :: Gen CFilterAction
        return $ performCFilter x 


--instance Arbitrary EncodingDecl where
--   arbitrary = oneof $ map (return . EncodingDecl) ["UTF-8"]
-- 
--instance Arbitrary XMLDecl where
--   arbitrary = do 
--                ver <- oneof $ map return ["1.0"]
--                (x,y) <- arbitrary
--                return $ XMLDecl ver x y
--

instance Arbitrary String where
   --arbitrary = genName
   arbitrary = mgenName 

data MXml = MXml (Document Posn) deriving Show

$(devArbitrary ''MXml)
$(devMutation ''MXml)

--mgen :: [Int] -> Gen MXml
--mgen = customGen_Xml_MXml

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
 
mdecode :: C8.ByteString -> MXml
mdecode xml =  MXml $ xmlParse "" (C8.unpack xml) 
              -- unsafePerformIO $ catch ( evaluate $ Just $ xmlParse "" (C8.unpack xml)) mhandler1
