{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}
module Ogg where

import DeriveArbitrary
import DeriveMArbitrary
import DeriveShow

import Test.QuickCheck

import Codec.Container.Ogg.Serial
import Codec.Container.Ogg.Page
import Codec.Container.Ogg.Packet
import Codec.Container.Ogg.Granulepos
import Codec.Container.Ogg.Track
import Codec.Container.Ogg.MessageHeaders
import Codec.Container.Ogg.Granulerate
import Codec.Container.Ogg.ContentType

import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy hiding (map)

--import Data.DeriveTH

import ByteString

$(devActions "Codec.Container.Ogg.ContentType" ''ContentType False [])
$(devArbitrary ''ContentTypeAction)
$(devArbitraryWithActions False ''ContentType)

$(devActions "Codec.Container.Ogg.Track" ''OggTrack False [])
$(devArbitrary ''OggTrackAction)
$(devArbitraryWithActions False ''OggTrack)

-- $(devActions "Codec.Container.Ogg.Packet" ''OggPacket False []) --BUG!!! GhcMod is unpacking definition on nested record
-- $(devArbitrary ''OggPacketAction)
-- $(devArbitraryWithActions False ''OggPacket)

data MOgg = Ogg0 [OggPacket] | Ogg1 [OggPage]

$(devArbitrary ''MOgg)
$(devShow ''MOgg)

--appendvorbis d = L.append flacIdent d
--appendh (OggPage x track cont incplt bos eos gp seqno s) = OggPage x track cont incplt bos eos gp seqno (Prelude.map appendvorbis s)

mencode (Ogg0 xs) = (L.concat . (map pageWrite) . packetsToPages) xs --appendvorbis
mencode (Ogg1 xs) = (L.concat . (map pageWrite) . packetsToPages . pagesToPackets) xs --appendvorbis
