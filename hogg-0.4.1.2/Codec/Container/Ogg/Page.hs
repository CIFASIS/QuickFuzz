--
-- Module      : Page
-- Copyright   : (c) Conrad Parker 2006
-- License     : BSD-style
-- Maintainer  : conradp@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable

module Codec.Container.Ogg.Page (
  OggPage (..),
  pageScan,
  pageWrite,
  pageLength,
  pageCompletedPackets,
  pageKeyGranule
) where

import Codec.Container.Ogg.ByteFields
import Codec.Container.Ogg.ContentType
import Codec.Container.Ogg.RawPage
import Codec.Container.Ogg.CRC
import Codec.Container.Ogg.Granulepos
import Codec.Container.Ogg.Serial
import Codec.Container.Ogg.Track
import Codec.Container.Ogg.Timestamp

import Data.List (find)
import Data.Int (Int64)
import Data.Maybe (maybeToList)
import Data.Word (Word8, Word32)
import Data.Bits
import qualified Data.ByteString.Lazy as L

import Text.Printf

------------------------------------------------------------
-- Data
--

data OggPage =
  OggPage {
    pageOffset :: !Int64,
    pageTrack :: !OggTrack,
    pageContinued :: !Bool,
    pageIncomplete :: !Bool,
    pageBOS :: !Bool,
    pageEOS :: !Bool,
    pageGranulepos :: !Granulepos,
    pageSeqno :: !Word32,
    pageSegments :: !([L.ByteString])
  }

------------------------------------------------------------
-- OggPage functions
--

-- | Determine the length in bytes of a page that would be written
pageLength :: OggPage -> Int
pageLength g = 27 + numsegs + sum (map (fromIntegral . L.length) s)
    where (numsegs, _) = buildSegtab 0 [] incplt s
          incplt = pageIncomplete g
          s = pageSegments g

-- | Determine the number of packets completed by this page
pageCompletedPackets :: OggPage -> Int
pageCompletedPackets g
  | pageIncomplete g = n-1
  | otherwise = n
  where n = length (pageSegments g)

-- | Determine the keygranule of a page
pageKeyGranule :: OggPage -> Maybe Integer
pageKeyGranule g = case (pageGranulepos g) of
  Granulepos Nothing -> Nothing
  gp@(Granulepos (Just _)) -> do
    let Just (k, _) = gpSplit gp (pageTrack g)
    return k

------------------------------------------------------------
-- Custom Instances
--

instance ContentTyped OggPage where
  contentTypeIs t g = contentTypeIs t (pageTrack g)
  contentTypeOf g = trackType (pageTrack g)

instance Serialled OggPage where
  serialOf g = serialOf (pageTrack g)

instance Timestampable OggPage where
  timestampOf g = gpToTimestamp gp track
    where
      gp = pageGranulepos g
      track = pageTrack g

------------------------------------------------------------
-- pageWrite
--

-- | Construct a binary representation of an Ogg page
pageWrite :: OggPage -> L.ByteString
pageWrite (OggPage _ track cont incplt bos eos gp seqno s) = newPageData
  where
    newPageData = L.concat [hData, crc, sData, body]
    crcPageData = L.concat [hData, zeroCRC, sData, body]
    hData = L.concat [pageMarker, version, htype, gp_, ser_, seqno_]
    sData = segs

    version = u8Fill pageVersion
    htype = L.pack [headerType]
    gp_ = le64Fill (gpUnpack gp)
    ser_ = le32Fill serialno
    seqno_ = le32Fill (0 :: Integer)--seqno
    crc = le32Fill (genCRC crcPageData)

    headerType :: Word8
    headerType = c .|. b .|. e
    c = if cont then (bit 0 :: Word8) else 0
    b = if bos then (bit 1 :: Word8) else 0
    e = if eos then (bit 2 :: Word8) else 0

    serialno = trackSerialno track

    -- Segment table
    segs = L.pack $ (fromIntegral numsegs):segtab
    (numsegs, segtab) = buildSegtab 0 [] incplt s

    -- Body data
    body = L.concat s

buildSegtab :: Int -> [Word8] -> Bool -> [L.ByteString] -> (Int, [Word8])
buildSegtab numsegs accum _ [] = (numsegs, accum)
buildSegtab numsegs accum incplt (x:xs) =
    buildSegtab (numsegs+length(tab)) (accum ++ tab) incplt xs
  where
    (q,r) = quotRem (fromIntegral $ L.length x) 255
    tab = buildTab q r xs incplt

buildTab :: Int -> Int -> [a] -> Bool -> [Word8]
buildTab 0 r _ _ = [fromIntegral r]
-- don't add [0] if the last seg is cont
buildTab q 0 [] True = take q $ repeat (255 :: Word8)
buildTab q r _ _ = ((take q $ repeat (255 :: Word8)) ++ [fromIntegral r])

------------------------------------------------------------
-- pageScan
--

-- | Read a list of data bytes into Ogg pages
pageScan :: L.ByteString -> ([OggTrack], [OggPage], L.ByteString)
pageScan = pageScan' True 0 []

pageScan' :: Bool -> Int64 -> [OggTrack] -> L.ByteString
          -> ([OggTrack], [OggPage], L.ByteString)
pageScan' allowBOS offset tracks input
  | L.null input                  = ([], [], L.empty)
  | L.isPrefixOf pageMarker input = pageResult
  | otherwise                     = pageScan' allowBOS (offset+1) tracks (L.tail input)
  where
    pageResult = pageProcess offset tracks $ pageBuild allowBOS offset tracks input

-- | Process the output of pageBuild, interpret as either the end of a chain
-- or the construction of a new page in the current chain
pageProcess :: Int64 -> [OggTrack]
            -> Either L.ByteString (OggPage, Int64, L.ByteString, Maybe OggTrack, Bool) -- as returned by pageBuild
            -> ([OggTrack], [OggPage], L.ByteString) -- to return from pageScan'
pageProcess _ _ (Left rest) = ([], [], rest)
pageProcess offset tracks (Right (newPage, pageLen, rest, mNewTrack, aBOS)) =
  (lNewTrack ++ nextTracks, newPage : nextPages, nextRest)
  where
    (nextTracks, nextPages, nextRest) = pageScan' aBOS (offset+pageLen) newTracks rest
    lNewTrack = maybeToList mNewTrack
    newTracks = lNewTrack ++ tracks

-- | Parse the given ByteString, and either detect the end of the chain, or
-- build one OggPage data structure
pageBuild :: Bool -> Int64 -> [OggTrack] -> L.ByteString ->
  Either
    L.ByteString -- The ByteString corresponding to the start of the next chain
    (OggPage, -- The constructed OggPage data structure
     Int64, -- The length of the page in bytes
     L.ByteString, -- The data following the page
     Maybe OggTrack, -- Maybe a new track, if this page started a new track
     Bool -- Whether or not to allow BOS after this (ie. not-seen-non-BOS?)
    )
pageBuild allowBOS o t d = buildResult allowBOS bos where

  buildResult True _ = Right (newPage, pageLen, rest, mNewTrack, bos)
  buildResult False False = Right (newPage, pageLen, rest, mNewTrack, False)
  buildResult False True = Left d

  newPage = OggPage o track cont incplt bos eos gp seqno segments
  (r, pageLen) = rawPageBuild d
  htype = rawPageHType r
  (mNewTrack, track) = findOrAddTrack serialno body t
  cont = testBit htype 0
  incplt = (not . null) segtab && last segtab == 255
  bos = testBit htype 1
  eos = testBit htype 2
  gp = gpPack (rawPageGranulepos r)
  serialno = rawPageSerialno r
  seqno = rawPageSeqno r
  segtab = rawPageSegtab r
  body = rawPageBody r
  segments = splitSegments 0 segtab body
  rest = L.drop pageLen d 

findOrAddTrack :: Serial -> L.ByteString -> [OggTrack] -> (Maybe OggTrack, OggTrack)
findOrAddTrack s d t = foat fTrack
  where
    fTrack = find (\x -> trackSerialno x == s) t
    foat :: Maybe OggTrack -> (Maybe OggTrack, OggTrack)
    foat (Just track) = (Nothing, track)
    foat Nothing      = (Just bTrack, bTrack)
    bTrack = bosToTrack s d

-- splitSegments accum segtab body
splitSegments :: Int -> [Int] -> L.ByteString -> [L.ByteString]
splitSegments 0 [0] _ = [L.empty]
splitSegments accum segments body
  | L.null body          = []
  -- accum == 0 &&  L.null segments = []
  | null segments        = [L.take (fromIntegral accum) body]
  | accum == 0 && l == 0 = L.empty : splitSegments 0 ls body
  | l == 255             = splitSegments (accum+255) ls body
  | otherwise            = newseg : splitSegments 0 ls newbody
  where (newseg, newbody) = L.splitAt (fromIntegral (accum+l)) body
        (l:ls) = segments

------------------------------------------------------------
-- Ordering
--

instance Eq OggPage where
  (==) g1 g2 = (==) t1 t2
    where t1 = timestampOf g1
          t2 = timestampOf g2

instance Ord OggPage where
  compare g1 g2
    | pageBOS g1 = LT
    | pageBOS g2 = GT
    | otherwise = compare t1 t2
    where t1 = timestampOf g1
          t2 = timestampOf g2

------------------------------------------------------------
-- Show
--

instance Show OggPage where
  show g@(OggPage o track cont incplt bos eos gp _ segment_table) =
    off ++ ": " ++ t ++ " serialno " ++ show (trackSerialno track) ++ ", granulepos " ++ gpe ++ flags ++ ": " ++ show (pageLength g) ++ " bytes\n" ++ "\t" ++ show (map L.length segment_table) ++ " " ++ ts ++ "\n" ++ "\n"
    where gpe = gpExplain gp track
          flags = ifc ++ ift ++ ifb ++ ife
          ifc = if cont then " (cont)" else ""
          ift = if incplt then " (incplt)" else ""
          ifb = if bos then " *** bos" else ""
          ife = if eos then " *** eos" else ""
          off = printf "0x%08x" ((fromIntegral o) :: Int)
          ts = maybe "--:--:--::--" show (timestampOf g)
          t = maybe "(Unknown)" show (trackType track)
