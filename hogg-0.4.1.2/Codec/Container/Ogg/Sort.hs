--
-- Module      : Sort
-- Copyright   : (c) Conrad Parker 2008
-- License     : BSD-style
-- Maintainer  : conradp@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable

module Codec.Container.Ogg.Sort (
  merge,
  sort,
  --, sortHeaders
) where

import Codec.Container.Ogg.ContentType
import Codec.Container.Ogg.List
import Codec.Container.Ogg.Headers
import Codec.Container.Ogg.Page
import Codec.Container.Ogg.Serial
import Codec.Container.Ogg.Track

------------------------------------------------------------
-- Exposed functions
--

merge :: [[OggPage]] -> [OggPage]
merge = sortHeaders . mergeSkeleton . listMerge

sort :: [OggPage] -> [OggPage]
sort = sortHeaders . listMerge . demux

------------------------------------------------------------
-- sortHeaders
--

-- | Ensure the header pages of each track are in the correct order
--   relative to each other.
sortHeaders :: [OggPage] -> [OggPage]
sortHeaders = processHeaders (sortHeaders' [] [] [] [] [])

sortHeaders' :: [OggPage] -- skeleton bos
             -> [OggPage] -- theora bos
             -> [OggPage] -- other bos
             -> [OggPage] -- other headers
             -> [OggPage] -- skeleton eos
             -> [OggPage] -- input
             -> [OggPage] -- output
sortHeaders' sb tb ob oh se [] = sb ++ tb ++ ob ++ oh ++ se
sortHeaders' sb tb ob oh se (g:gs)
  | contentTypeIs skeleton g = case (pageBOS g, pageEOS g) of
    (True, False) -> sortHeaders' (sb++[g]) tb ob oh se gs
    (False, True) -> sortHeaders' sb tb ob oh (se++[g]) gs
    _             -> sortHeaders' sb tb ob (oh++[g]) se gs
  | contentTypeIs theora g = case (pageBOS g) of
    True          -> sortHeaders' sb (tb++[g]) ob oh se gs
    False         -> sortHeaders' sb tb ob (oh++[g]) se gs
  | otherwise = case (pageBOS g) of
    True          -> sortHeaders' sb tb (ob++[g]) oh se gs
    False         -> sortHeaders' sb tb ob (oh++[g]) se gs

------------------------------------------------------------
-- mergeSkeleton
--

-- | When merging multiple files together, ensure that the resulting file
--   contains only one Skeleton track
mergeSkeleton :: [OggPage] -> [OggPage]
mergeSkeleton [] = []
mergeSkeleton (g:gs) 
  | contentTypeIs skeleton g = g : mergeSkeleton' (pageTrack g) gs
  | otherwise = g : mergeSkeleton gs 

mergeSkeleton' :: OggTrack -> [OggPage] -> [OggPage]
mergeSkeleton' _ [] = []
mergeSkeleton' t' (g:gs)
  | contentTypeIs skeleton g = case (t == t', pageBOS g, pageEOS g) of
      (True, _, _)     -> g:gs'  -- Include known Skeleton pages
      (False, True, _) -> gs'    -- Drop other Skeleton BOS pages
      (False, _, True) -> gs'    -- Drop other Skeleton EOS pages
      (False, _, _)    -> g':gs' -- Fix serialno of other Skeleton pages
  | otherwise = g : gs'          -- Pass non-Skeleton pages through
  where
    t = pageTrack g
    g' = g{pageTrack = t'}
    gs' = mergeSkeleton' t' gs 
