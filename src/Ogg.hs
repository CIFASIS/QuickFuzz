{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}
module Ogg where

import Args
import Check
import DeriveArbitrary
import Test.QuickCheck

import Data.Binary( Binary(..), encode )

import Codec.Container.Ogg.Page
import Codec.Container.Ogg.Granulepos
import Codec.Container.Ogg.Track
import Codec.Container.Ogg.MessageHeaders
import Codec.Container.Ogg.Granulerate
import Codec.Container.Ogg.ContentType

import qualified Data.ByteString.Lazy as L

import Data.DeriveTH
import Data.Word(Word8, Word16, Word32)
import Data.Int( Int16, Int8 )

import Data.Binary.Put( runPut )

import ByteString

import Data.List.Split

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

oggmain (MainArgs _ cmd filename prop maxSuccess maxSize outdir _) = let (prog, args) = (head spl, tail spl) in
    (case prop of
        "zzuf" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ zzufprop filename prog args mencode outdir)
        "check" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ checkprop filename prog args mencode outdir)
        "gen" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ genprop filename prog args mencode outdir)
        "exec" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ execprop filename prog args mencode outdir)
        _     -> error "Invalid action selected"
    ) where spl = splitOn " " cmd

main fargs False = oggmain $ fargs ""
main fargs True  = processPar fargs oggmain
