--
-- Module      : RawPage
-- Copyright   : (c) Conrad Parker 2006
-- License     : BSD-style
-- Maintainer  : conradp@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable

module Codec.Container.Ogg.RawPage (
  OggRawPage (..),
  pageMarker,
  pageVersion,
  rawPageScan,
  rawPageBuild
) where

import Codec.Container.Ogg.ByteFields

import qualified Data.ByteString.Lazy as L
import Data.Int (Int64)
import Data.Word (Word8, Word32, Word64)

------------------------------------------------------------
-- The Ogg page format
-- from RFC3533: http://www.ietf.org/rfc/rfc3533.txt
{-

 0                   1                   2                   3
 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1| Byte
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
| capture_pattern: Magic number for page start "OggS"           | 0-3
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
| version       | header_type   | granule_position              | 4-7
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                                                               | 8-11
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                               | bitstream_serial_number       | 12-15
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                               | page_sequence_number          | 16-19
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                               | CRC_checksum                  | 20-23
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                               |page_segments  | segment_table | 24-27
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
| ...                                                           | 28-
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

-}

------------------------------------------------------------
-- Data
--

data OggRawPage =
  OggRawPage {
    rawPageVersion :: !Word8,
    rawPageHType :: !Word8,
    rawPageGranulepos :: !Word64,
    rawPageSerialno :: !Word32,
    rawPageSeqno :: !Word32,
    rawPageCRC :: !Word32,
    rawPageNumseg :: !Int,
    rawPageSegtab :: !([Int]),
    rawPageBody :: !L.ByteString
  }

------------------------------------------------------------
-- OggRawPage constants
--

pageMarker :: L.ByteString
pageMarker = L.pack [0x4f, 0x67, 0x67, 0x53] -- "OggS"

-- | Ogg version supported by this library
pageVersion :: Word8
pageVersion = 0x00

------------------------------------------------------------
-- rawPageScan
--

rawPageScan :: L.ByteString -> [OggRawPage]
rawPageScan input
  | L.null input                  = []
  | L.isPrefixOf pageMarker input = newPage : rawPageScan rest
  | otherwise                     = rawPageScan (L.tail input)
  where (newPage, pageLen) = rawPageBuild input
        rest = L.drop pageLen input

rawPageBuild :: L.ByteString -> (OggRawPage, Int64)
rawPageBuild d = (newRawPage, pageLen) where
  newRawPage = OggRawPage v htype gp serialno seqno crc numseg segtab body
  v = u8At 4 d
  htype = u8At 5 d
  gp = le64At 6 d
  serialno = le32At 14 d
  seqno = le32At 18 d
  crc = le32At 22 d
  numseg64 = u8At 26 d
  numseg = fromIntegral numseg64
  st = L.unpack $ L.take numseg64 (L.drop 27 d)
  segtab = map fromIntegral st
  headerSize = 27 + numseg64
  bodySize = fromIntegral $ sum segtab
  body = L.take bodySize (L.drop headerSize d)
  pageLen = fromIntegral $ headerSize + bodySize

------------------------------------------------------------
-- Show
--

instance Show OggRawPage where
  show r =
    "Version: " ++ show (rawPageVersion r) ++ "\n" ++
    "HType: " ++ show (rawPageHType r) ++ "\n" ++
    "Granulepos: " ++ show (rawPageGranulepos r) ++ "\n" ++
    "Serialno: " ++ show (rawPageSerialno r) ++ "\n" ++
    "Seqno: " ++ show (rawPageSeqno r) ++ "\n" ++
    "CRC: " ++ show (rawPageCRC r) ++ "\n" ++
    "Numseg: " ++ show (rawPageNumseg r) ++ "\n" ++
    "Segtab: " ++ show (rawPageSegtab r) ++ "\n" ++ "\n"
