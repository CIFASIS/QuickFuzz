{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}
module Ogg where

import DeriveArbitrary
import Test.QuickCheck

import Codec.Container.Ogg.Page
import Codec.Container.Ogg.Granulepos
import Codec.Container.Ogg.Track
import Codec.Container.Ogg.MessageHeaders
import Codec.Container.Ogg.Granulerate
import Codec.Container.Ogg.ContentType

import qualified Data.ByteString.Lazy as L

import Data.DeriveTH

import ByteString

derive makeArbitrary ''OggPage
derive makeArbitrary ''Granulepos
derive makeArbitrary ''OggTrack
derive makeArbitrary ''Granulerate

instance Arbitrary ContentType where
   arbitrary = oneof $ (map return [skeleton, cmml, vorbis, theora, speex, celt, flac])

instance Arbitrary MessageHeaders where
   arbitrary = do
     y <- listOf (arbitrary :: Gen String)
     x <- (arbitrary :: (Gen String))
     return $ mhAppends x y mhEmpty

appendvorbis d = L.append theoraIdent d
appendh (OggPage x track cont incplt bos eos gp seqno s) = OggPage x track cont incplt bos eos gp seqno (map appendvorbis s)

mencode = appendvorbis
