{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Zip where

import DeriveArbitrary
import DeriveMArbitrary
import DeriveMutation
import Data.Binary( Binary(..), encode )
import Codec.Archive.Zip

--import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy
import Data.Word
--import Vector
--import ByteString
import Test.QuickCheck


-- data MArchive = Archive0 Archive

$(devActions ["Codec.Archive.Zip"] ''Entry False [])
$(devArbitrary ''EntryAction)
$(devArbitraryWithActions False ''Entry)


$(devActions ["Codec.Archive.Zip"] ''Archive False [])
$(devArbitrary ''ArchiveAction)
$(devArbitraryWithActions False ''Archive)


-- $(devArbitrary ''Archive)
-- $(devMutationRec ''MArchive)
-- $(devMutation ''Entry Nothing)
-- $(devMutation ''L.ByteString Nothing)
-- $(devMutation ''Data.ByteString.Internal.ByteString Nothing)
-- $(devMutationRec ''MArchive)

mencode :: Archive -> ByteString
mencode = encode
--mencode (Archive1 xs) = encode $ foldr addEntryToArchive emptyArchive (map (\(x,y,z) -> toEntry x y z) xs) 
