--
-- Module      : Dump
-- Copyright   : (c) Conrad Parker 2006
-- License     : BSD-style
-- Maintainer  : conradp@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable

module Codec.Container.Ogg.Dump (
  hexDump
) where

import Data.Char (isSpace, chr, ord)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C

import Numeric

------------------------------------------------------------
-- Dump
--

-- | Generate a hexdump for a block of data

hexDump :: L.ByteString -> C.ByteString
hexDump = {-# SCC "hexDump" #-} hexDump' 0

hexDump' :: Int -> L.ByteString -> C.ByteString
hexDump' o d
  | L.null d = C.empty
  | otherwise = lineDump `C.append` hexDump' (o+16) rest
    where (line, rest) = {-# SCC "LsplitAt" #-} L.splitAt 16 d
          lineDump = {-# SCC "Cpack" #-} C.pack $ {-# SCC "lineDump" #-} spaces 4 ++ offset ++ ": " ++ hexLine ++ spaces hexPad ++ ascLine ++ "\n"
          spaces n = {-# SCC "spaces" #-} take n $ repeat ' '
          offset = {-# SCC "offset" #-} hexOffset o

          (hexLine, ascLine) = {-# SCC "makeDump" #-} makeDump line False
          hexPad = {-# SCC "hexPad" #-} 1 + 8*5 - length hexLine

          hexOffset x
            | x < 0x10   = showHex x "000"
            | x < 0x100  = showHex x "00"
            | x < 0x1000 = showHex x "0"
            | otherwise  = showHex x ""

makeDump :: L.ByteString -> Bool -> (String, String)
makeDump d hexSpace
  | L.null d = ([], [])
  | otherwise = (newHex ++ nextHex, newChr ++ nextChr)
    where (nextHex, nextChr) = {-# SCC "makeDumpREC" #-} makeDump ls (not hexSpace)
          (l, ls) = {-# SCC "LHeadTail" #-} (L.head d, L.tail d)
          newHex = {-# SCC "newHex" #-} if hexSpace then h ++ " " else h
          h = hexByte l
          newChr = {-# SCC "newChr" #-} ascByte $ chr $ fromIntegral l
          hexByte x
            | x < 16    = showHex x "0"
            | otherwise = showHex x ""
          ascByte c
            | (ord c) > 126 = "."
            | isSpace c = " "
            | (ord c) < 32 = "."
            | otherwise = [c]
