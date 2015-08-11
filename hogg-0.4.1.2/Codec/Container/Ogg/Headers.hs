--
-- Module      : Headers
-- Copyright   : (c) Conrad Parker 2008
-- License     : BSD-style
-- Maintainer  : conradp@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable

module Codec.Container.Ogg.Headers (
  processHeaders
) where

import qualified Data.Map as Map

import Codec.Container.Ogg.ContentType
import Codec.Container.Ogg.Page
import Codec.Container.Ogg.Track

------------------------------------------------------------
-- processHeaders
--

processHeaders :: ([OggPage] -> [OggPage]) -> [OggPage] -> [OggPage]
processHeaders f gs = (f controlSection) ++ dataSection
  where (controlSection, dataSection) = splitHeaders gs

-- | Separate out the control and data sections
splitHeaders :: [OggPage] -> ([OggPage], [OggPage])
splitHeaders gs = splitHeaders' Map.empty ([], [], gs)

splitHeaders' ::
              -- Pages seen so far, demuxed and keyed by Track
              Map.Map OggTrack [OggPage]

              -- (headers, seenNotHeaders, notSeen)
              -> ([OggPage], [OggPage], [OggPage])

              -- output
              -> ([OggPage], [OggPage])

splitHeaders' _ (hs, nhs, []) = (hs, nhs)

splitHeaders' seen (hs, nhs, (g:gs))
  -- Include Skeleton pages in the control section, and don't bother keeping
  -- track of them in the list of seen pages
  | isSkeleton = splitHeaders' seen (hs++[g], nhs, gs)

  -- If this is a BOS page, add it to the list of seen tracks
  | pageBOS g = splitHeaders' insertSeen (hs++[g], nhs, gs)

  -- If all headers have been processed, return the remaining pages unmodified
  | Map.null seen = (hs, nhs++(g:gs))

  -- If this track has already been fully processed, mark this as not a header
  | notSeen = splitHeaders' seen (hs, nhs++[g], gs)

  -- If this is the last header for this track, remove it from the list
  | lastHeader = splitHeaders' deleteSeen (hs++[g], nhs, gs)

  -- Otherwise, g is a header
  | otherwise = splitHeaders' appendSeen (hs++[g], nhs, gs)

  where
    t = pageTrack g
    insertSeen = Map.insert t [g] seen
    appendSeen = Map.insert t tPages seen
    deleteSeen = Map.delete t seen

    notSeen = Map.notMember t seen

    isSkeleton = contentTypeIs skeleton t

    lastHeader = nPackets >= nHeaders
    nHeaders = trackHeaders t
    nPackets = sum $ map pageCompletedPackets tPages
    tPages = (Map.findWithDefault [] t seen) ++ [g]
