{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Archive.Tar.Read
-- Copyright   :  (c) 2007 Bjorn Bringert,
--                    2008 Andrea Vezzosi,
--                    2008-2009 Duncan Coutts,
--                    2011 Max Bolingbroke
-- License     :  BSD3
--
-- Maintainer  :  duncan@community.haskell.org
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Codec.Archive.Tar.Read (read, FormatError(..)) where

import Codec.Archive.Tar.Types

import Data.Char     (ord)
import Data.Int      (Int64)
import Numeric       (readOct)
import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Control.Applicative
import Control.Monad

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS.Char8
import Data.ByteString.Lazy (ByteString)

import Prelude hiding (read)


-- | Errors that can be encountered when parsing a Tar archive.
data FormatError
  = TruncatedArchive
  | ShortTrailer
  | BadTrailer
  | TrailingJunk
  | ChecksumIncorrect
  | NotTarFormat
  | UnrecognisedTarFormat
  | HeaderBadNumericEncoding
  deriving (Typeable)

instance Show FormatError where
  show TruncatedArchive         = "truncated tar archive"
  show ShortTrailer             = "short tar trailer"
  show BadTrailer               = "bad tar trailer"
  show TrailingJunk             = "tar file has trailing junk"
  show ChecksumIncorrect        = "tar checksum error"
  show NotTarFormat             = "data is not in tar format"
  show UnrecognisedTarFormat    = "tar entry not in a recognised format"
  show HeaderBadNumericEncoding = "tar header is malformed (bad numeric encoding)"

instance Exception FormatError


-- | Convert a data stream in the tar file format into an internal data
-- structure. Decoding errors are reported by the 'Fail' constructor of the
-- 'Entries' type.
--
-- * The conversion is done lazily.
--
read :: ByteString -> Entries FormatError
read = unfoldEntries getEntry

getEntry :: ByteString -> Either FormatError (Maybe (Entry, ByteString))
getEntry bs
  | BS.length header < 512 = Left TruncatedArchive

  -- Tar files end with at least two blocks of all '0'. Checking this serves
  -- two purposes. It checks the format but also forces the tail of the data
  -- which is necessary to close the file if it came from a lazily read file.
  | BS.head bs == 0 = case BS.splitAt 1024 bs of
      (end, trailing)
        | BS.length end /= 1024        -> Left ShortTrailer
        | not (BS.all (== 0) end)      -> Left BadTrailer
        | not (BS.all (== 0) trailing) -> Left TrailingJunk
        | otherwise                    -> Right Nothing

  | otherwise  = partial $ do

  case (chksum_, format_) of
    (Ok chksum, _   ) | correctChecksum header chksum -> return ()
    (Ok _,      Ok _) -> Error ChecksumIncorrect
    _                 -> Error NotTarFormat

  -- These fields are partial, have to check them
  format   <- format_;   mode     <- mode_;
  uid      <- uid_;      gid      <- gid_;
  size     <- size_;     mtime    <- mtime_;
  devmajor <- devmajor_; devminor <- devminor_;

  let content = BS.take size (BS.drop 512 bs)
      padding = (512 - size) `mod` 512
      bs'     = BS.drop (512 + size + padding) bs

      entry = Entry {
        entryTarPath     = TarPath name prefix,
        entryContent     = case typecode of
                   '\0' -> NormalFile      content size
                   '0'  -> NormalFile      content size
                   '1'  -> HardLink        (LinkTarget linkname)
                   '2'  -> SymbolicLink    (LinkTarget linkname)
                   '3'  -> CharacterDevice devmajor devminor
                   '4'  -> BlockDevice     devmajor devminor
                   '5'  -> Directory
                   '6'  -> NamedPipe
                   '7'  -> NormalFile      content size
                   _    -> OtherEntryType  typecode content size,
        entryPermissions = mode,
        entryOwnership   = Ownership uname gname uid gid,
        entryTime        = mtime,
        entryFormat      = format
    }

  return (Just (entry, bs'))

  where
   header = BS.take 512 bs

   name       = getString   0 100 header
   mode_      = getOct    100   8 header
   uid_       = getOct    108   8 header
   gid_       = getOct    116   8 header
   size_      = getOct    124  12 header
   mtime_     = getOct    136  12 header
   chksum_    = getOct    148   8 header
   typecode   = getByte   156     header
   linkname   = getString 157 100 header
   magic      = getChars  257   8 header
   uname      = getString 265  32 header
   gname      = getString 297  32 header
   devmajor_  = getOct    329   8 header
   devminor_  = getOct    337   8 header
   prefix     = getString 345 155 header
-- trailing   = getBytes  500  12 header

   format_ = case magic of
    "\0\0\0\0\0\0\0\0" -> return V7Format
    "ustar\NUL00"      -> return UstarFormat
    "ustar  \NUL"      -> return GnuFormat
    _                  -> Error UnrecognisedTarFormat

correctChecksum :: ByteString -> Int -> Bool
correctChecksum header checksum = checksum == checksum'
  where
    -- sum of all 512 bytes in the header block,
    -- treating each byte as an 8-bit unsigned value
    checksum' = BS.Char8.foldl' (\x y -> x + ord y) 0 header'
    -- treating the 8 bytes of chksum as blank characters.
    header'   = BS.concat [BS.take 148 header,
                           BS.Char8.replicate 8 ' ',
                           BS.drop 156 header]

-- * TAR format primitive input

getOct :: Integral a => Int64 -> Int64 -> ByteString -> Partial FormatError a
getOct off len = parseOct
               . BS.Char8.unpack
               . BS.Char8.takeWhile (\c -> c /= '\NUL' && c /= ' ')
               . BS.Char8.dropWhile (== ' ')
               . getBytes off len
  where
    parseOct "" = return 0
    -- As a star extension, octal fields can hold a base-256 value if the high
    -- bit of the initial character is set. The initial character can be:
    --   0x80 ==> trailing characters hold a positive base-256 value
    --   0xFF ==> trailing characters hold a negative base-256 value
    --
    -- In both cases, there won't be a trailing NUL/space.
    --
    -- GNU tar seems to contain a half-implementation of code that deals with
    -- extra bits in the first character, but I don't think it works and the
    -- docs I can find on star seem to suggest that these will always be 0,
    -- which is what I will assume.
    parseOct ('\128':xs) = return (readBytes xs)
    parseOct ('\255':xs) = return (negate (readBytes xs))
    parseOct s  = case readOct s of
      [(x,[])] -> return x
      _        -> Error HeaderBadNumericEncoding

    readBytes = go 0
      where go acc []     = acc
            go acc (x:xs) = go (acc * 256 + fromIntegral (ord x)) xs

getBytes :: Int64 -> Int64 -> ByteString -> ByteString
getBytes off len = BS.take len . BS.drop off

getByte :: Int64 -> ByteString -> Char
getByte off bs = BS.Char8.index bs off

getChars :: Int64 -> Int64 -> ByteString -> String
getChars off len = BS.Char8.unpack . getBytes off len

getString :: Int64 -> Int64 -> ByteString -> String
getString off len = BS.Char8.unpack . BS.Char8.takeWhile (/='\0') . getBytes off len

-- These days we'd just use Either, but in older versions of base there was no
-- Monad instance for Either, it was in mtl with an anoying Error constraint.
--
data Partial e a = Error e | Ok a

partial :: Partial e a -> Either e a
partial (Error msg) = Left msg
partial (Ok x)      = Right x

instance Functor (Partial e) where
    fmap = liftM

instance Applicative (Partial e) where
    pure  = return
    (<*>) = ap

instance Monad (Partial e) where
    return        = Ok
    Error m >>= _ = Error m
    Ok    x >>= k = k x
    fail          = error "fail @(Partial e)"
