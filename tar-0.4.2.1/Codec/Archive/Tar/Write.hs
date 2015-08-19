-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Archive.Tar.Write
-- Copyright   :  (c) 2007 Bjorn Bringert,
--                    2008 Andrea Vezzosi,
--                    2008-2009 Duncan Coutts
-- License     :  BSD3
--
-- Maintainer  :  duncan@community.haskell.org
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Codec.Archive.Tar.Write (write) where

import Codec.Archive.Tar.Types

import Data.Char     (ord)
import Data.List     (foldl')
import Numeric       (showOct)

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS.Char8
import Data.ByteString.Lazy (ByteString)


-- | Create the external representation of a tar archive by serialising a list
-- of tar entries.
--
-- * The conversion is done lazily.
--
write :: [Entry] -> ByteString
write es = BS.concat $ map putEntry es ++ [BS.replicate (512*2) 0]

putEntry :: Entry -> ByteString
putEntry entry = case entryContent entry of
  NormalFile       content size -> BS.concat [ header, content, padding size ]
  OtherEntryType _ content size -> BS.concat [ header, content, padding size ]
  _                             -> header
  where
    header       = putHeader entry
    padding size = BS.replicate paddingSize 0
      where paddingSize = fromIntegral (negate size `mod` 512)

putHeader :: Entry -> ByteString
putHeader entry =
     BS.Char8.pack $ take 148 block
  ++ putOct 7 checksum
  ++ ' ' : drop 156 block
--  ++ putOct 8 checksum
--  ++ drop 156 block
  where
    block    = putHeaderNoChkSum entry
    checksum = foldl' (\x y -> x + ord y) 0 block

putHeaderNoChkSum :: Entry -> String
putHeaderNoChkSum Entry {
    entryTarPath     = TarPath name prefix,
    entryContent     = content,
    entryPermissions = permissions,
    entryOwnership   = ownership,
    entryTime        = modTime,
    entryFormat      = format
  } =

  concat
    [ putString  100 $ name
    , putOct       8 $ permissions
    , putOct       8 $ ownerId ownership
    , putOct       8 $ groupId ownership
    , putOct      12 $ contentSize
    , putOct      12 $ modTime
    , fill         8 $ ' ' -- dummy checksum
    , putChar8       $ typeCode
    , putString  100 $ linkTarget
    ] ++
  case format of
  V7Format    ->
      fill 255 '\NUL'
  UstarFormat -> concat
    [ putString    8 $ "ustar\NUL00"
    , putString   32 $ ownerName ownership
    , putString   32 $ groupName ownership
    , putOct       8 $ deviceMajor
    , putOct       8 $ deviceMinor
    , putString  155 $ prefix
    , fill        12 $ '\NUL'
    ]
  GnuFormat -> concat
    [ putString    8 $ "ustar  \NUL"
    , putString   32 $ ownerName ownership
    , putString   32 $ groupName ownership
    , putGnuDev    8 $ deviceMajor
    , putGnuDev    8 $ deviceMinor
    , putString  155 $ prefix
    , fill        12 $ '\NUL'
    ]
  where
    (typeCode, contentSize, linkTarget,
     deviceMajor, deviceMinor) = case content of
       NormalFile      _ size            -> ('0' , size, [],   0,     0)
       Directory                         -> ('5' , 0,    [],   0,     0)
       SymbolicLink    (LinkTarget link) -> ('2' , 0,    link, 0,     0)
       HardLink        (LinkTarget link) -> ('1' , 0,    link, 0,     0)
       CharacterDevice major minor       -> ('3' , 0,    [],   major, minor)
       BlockDevice     major minor       -> ('4' , 0,    [],   major, minor)
       NamedPipe                         -> ('6' , 0,    [],   0,     0)
       OtherEntryType  code _ size       -> (code, size, [],   0,     0)

    putGnuDev w n = case content of
      CharacterDevice _ _ -> putOct w n
      BlockDevice     _ _ -> putOct w n
      _                   -> replicate w '\NUL'

-- * TAR format primitive output

type FieldWidth = Int

putString :: FieldWidth -> String -> String
putString n s = take n s ++ fill (n - length s) '\NUL'

--TODO: check integer widths, eg for large file sizes
putOct :: (Integral a, Show a) => FieldWidth -> a -> String
putOct n x =
  let octStr = take (n-1) $ showOct x ""
   in fill (n - length octStr - 1) '0'
   ++ octStr
   ++ putChar8 '\NUL'

putChar8 :: Char -> String
putChar8 c = [c]

fill :: FieldWidth -> Char -> String
fill n c = replicate n c
