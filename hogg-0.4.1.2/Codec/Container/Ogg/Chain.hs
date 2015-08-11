--
-- Module      : Codec.Container.Ogg.Chain
-- Copyright   : (c) Conrad Parker 2006
-- License     : BSD-style
-- Maintainer  : conradp@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable

module Codec.Container.Ogg.Chain (
  OggChain (..),
  chainScan,
  chainAddSkeleton
) where

import qualified Data.ByteString.Lazy as L

import Codec.Container.Ogg.ContentType
import Codec.Container.Ogg.Granulepos
import Codec.Container.Ogg.Track
import Codec.Container.Ogg.Page
import Codec.Container.Ogg.Packet
import Codec.Container.Ogg.Serial
import Codec.Container.Ogg.Skeleton

-- | A section of a chained Ogg physical bitstream. This corresponds to
-- an entire song or video, and most Ogg files in the wild contain only
-- a single chain.
data OggChain =
  OggChain {
    chainTracks :: [OggTrack],
    chainPages :: [OggPage],
    chainPackets :: [OggPacket]
  }
  
-- | Parse a ByteString into a list of OggChains
chainScan :: L.ByteString -> [OggChain]
chainScan d
  | L.null d  = []
  | otherwise = chain : chainScan rest
  where chain = OggChain tracks pages packets
        (tracks, pages, rest) = pageScan d
        packets = pagesToPackets pages

------------------------------------------------------------
-- chainAddSkeleton
--

-- | Add a Skeleton logical bitstream to an OggChain
chainAddSkeleton :: OggChain -> IO OggChain
chainAddSkeleton chain = do
  serialno <- genSerial
  return $ chainAddSkeleton' serialno chain

-- | Add a Skeleton logical bitstream with a given serialno to an OggChain
chainAddSkeleton' :: Serial -> OggChain -> OggChain
chainAddSkeleton' serialno (OggChain tracks _ packets) = OggChain nt ng np
  where
    nt = skelTrack : tracks
    ng = packetsToPages np
    np = fh : concat [ixBoss, ixFisbones, ixHdrs, [sEOS], ixD]

    -- Construct a new track for the Skeleton
    skelTrack = (newTrack serialno){trackType = Just skeleton}

    -- Create the fishead and fisbone packets (all with pageIx 0)
    fh = fisheadToPacket skelTrack emptyFishead
    fbs = map (fisboneToPacket skelTrack) $ tracksToFisbones tracks

    -- Separate out the BOS pages of the input
    (boss, rest) = span packetBOS packets

    -- Increment the pageIx of these original BOS pages by 1, as the
    -- Skeleton fishead packet is being prepended
    ixBoss = map (incPageIx 1) boss

    -- Split the remainder of the input into headers and data
    (hdrs, d) = splitAt totHeaders rest

    -- ... for which we determine the total number of header pages
    totHeaders = sum $ map trackHeaders tracks

    -- Increment the pageIx of the original data packets by the number of
    -- Skeleton pages
    ixHdrs = map (incPageIx (1 + length fbs)) hdrs
    ixD = map (incPageIx (2 + length fbs)) d

    -- Set the pageIx of the fisbones in turn, beginning after the last
    -- BOS page
    ixFisbones = zipWith setPageIx [1+(length tracks)..] fbs

    -- Generate an EOS packet for the Skeleton track
    sEOS = (uncutPacket L.empty skelTrack sEOSgp){packetEOS = True}
    sEOSgp = Granulepos (Just 0)

-- An internal function for setting the pageIx of the segment of a packet.
-- This is only designed for working with packets which are known to only
-- and entirely span one page, such as Skeleton fisbones.
setPageIx :: Int -> OggPacket -> OggPacket
setPageIx ix p@(OggPacket _ _ _ _ _ (Just [oldSegment])) =
  p{packetSegments = Just [newSegment]}
  where
    newSegment = oldSegment{segmentPageIx = ix}
setPageIx _ _ = error "setPageIx used on non-uncut page"

-- An internal function for incrementing the pageIx of all the segments of
-- a packet.
incPageIx :: Int -> OggPacket -> OggPacket
incPageIx ixd p@(OggPacket _ _ _ _ _ (Just segments)) =
  p{packetSegments = Just (map incSegIx segments)}
  where
    incSegIx :: OggSegment -> OggSegment
    incSegIx s@(OggSegment _ oix _) = s{segmentPageIx = oix + ixd}
-- Otherwise, the packet has no segmentation info so leave it untouched
incPageIx _ p = p
