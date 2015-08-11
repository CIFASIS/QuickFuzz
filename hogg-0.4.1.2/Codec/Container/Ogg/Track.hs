--
-- Module      : Track
-- Copyright   : (c) Conrad Parker 2006
-- License     : BSD-style
-- Maintainer  : conradp@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable

module Codec.Container.Ogg.Track (
  OggTrack (..),
  newTrack,
  nullTrack,
  bosToTrack,
  gpToTimestamp,
  gpToGranules,
  gpSplit,
  gpExplain,

  -- ContentTypeImplied typeclass
  ContentTypeImplied,
  contentTypeImplies
) where

import qualified Data.ByteString.Lazy as L
import Data.Bits
import Data.Ratio

import Text.Printf

import Codec.Container.Ogg.ContentType
import Codec.Container.Ogg.Granulepos
import Codec.Container.Ogg.Granulerate
import Codec.Container.Ogg.MessageHeaders
import Codec.Container.Ogg.Serial
import Codec.Container.Ogg.Timestamp

------------------------------------------------------------
-- Data
--

data OggTrack =
  OggTrack {
    trackSerialno :: Serial,
    trackType :: Maybe ContentType,
    trackHeaders :: Int,
    trackGranulerate :: Maybe Granulerate,
    trackGranuleshift :: Maybe Int,
    trackMetadata :: MessageHeaders
  }

------------------------------------------------------------
-- | Typeclass: ContentTypeImplied
--

class (ContentTyped a) => ContentTypeImplied a where
  contentTypeImplies :: [OggTrack] -> ContentType -> a -> Bool

------------------------------------------------------------
-- Instances: ContentTyped, ContentTypeImplied, Serialled
--

-- | Predicate
trackIsType :: ContentType -> OggTrack -> Bool
trackIsType t0 track
  | (Just t0) == t1  = True
  | otherwise        = False
  where t1 = trackType track

instance ContentTyped OggTrack where
  contentTypeIs = trackIsType
  contentTypeOf = trackType

instance ContentTypeImplied OggTrack where
  contentTypeImplies _ = trackIsType

instance Serialled OggTrack where
  serialOf = trackSerialno

------------------------------------------------------------
--
--

-- | The null track
nullTrack :: OggTrack
nullTrack = OggTrack 0 Nothing 0 Nothing Nothing mhEmpty

-- | A new track, with a given serialno
newTrack :: Serial -> OggTrack
newTrack serialno = OggTrack serialno Nothing 0 Nothing Nothing mhEmpty

-- Instantiate an OggTrack given a serialno and a bos page
bosToTrack :: Serial -> L.ByteString -> OggTrack
bosToTrack s d = OggTrack s ctype nh gr gs mh
  where
    ctype = identify d
    nh = maybe 1 (\x -> headers x d) ctype
    gr = maybe Nothing (\x -> granulerate x d) ctype
    gs = maybe Nothing (\x -> granuleshift x d) ctype
    mh = maybe mhEmpty (\x -> metadata x d) ctype

-- | Convert a granulepos to a timestamp
gpToTimestamp :: Granulepos -> OggTrack -> Maybe Timestamp
gpToTimestamp mgp track
  | g == Nothing = Nothing
  | r == Nothing = Nothing
  | otherwise    = Just (Timestamp timestamp)
  where g = gpToGranules mgp track
        r = trackGranulerate track
        timestamp = (granules*d, n)
        n = numerator gr
        d = denominator gr
        Just granules = g
        Just (Granulerate gr) = r

gpExplain :: Granulepos -> OggTrack -> String
gpExplain mgp track
  | s == Nothing = show mgp
  | otherwise    = show keyframe ++ "|" ++ show delta
  where s = gpSplit mgp track
        Just (keyframe, delta) = s

-- | Convert a granluepos to a count of granules
gpToGranules :: Granulepos -> OggTrack -> Maybe Integer
gpToGranules mgp track
  | s == Nothing = Nothing
  | otherwise    = Just (keyframe + delta)
  where s = gpSplit mgp track
        Just (keyframe, delta) = s

-- | Split a granulepos by its track's granuleshift
gpSplit :: Granulepos -> OggTrack -> Maybe (Integer, Integer)
gpSplit mgp track
  | mgp == Granulepos Nothing          = Nothing
  | trackGranuleshift track == Nothing = Just (gp, 0)
  | otherwise                          = Just (keyframe, delta)
  where Granulepos (Just w64gp) = mgp
        Just gShift = trackGranuleshift track
        gp = fromIntegral w64gp
        keyframe = fromIntegral $ w64gp `shiftR` gShift
        delta = fromIntegral $ gp - (keyframe `shiftL` gShift)

-- | Tracks are equal if their serialnos are equal
instance Eq OggTrack where
  (==) track1 track2 = s1 == s2
       where s1 = trackSerialno track1
             s2 = trackSerialno track2

instance Ord OggTrack where
  compare track1 track2 = compare s1 s2
          where s1 = trackSerialno track1
                s2 = trackSerialno track2

------------------------------------------------------------
-- Show
--

instance Show OggTrack where
  -- show (OggTrack serialno ctype gr gs mhdrs) =
  show (OggTrack serialno ctype _ _ _ mhdrs) =
    t ++ ": serialno " ++ s ++ "\n" ++ m
    where s = printf "%010u" ((fromIntegral serialno) :: Int)
          t = maybe "(Unknown)" show ctype
          m = unlines $ zipWith (++) (repeat "\t") (lines $ show mhdrs)

  -- show (OggTrack serialno _ _) =
  --   "(Unknown): serialno " ++ s ++ "\n"
  --   where s = printf "%010d" ((fromIntegral serialno) :: Int)
