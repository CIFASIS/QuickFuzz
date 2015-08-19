{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Archive.Tar
-- Copyright   :  (c) 2007 Bjorn Bringert,
--                    2008 Andrea Vezzosi,
--                    2008-2009 Duncan Coutts
-- License     :  BSD3
--
-- Maintainer  :  duncan@community.haskell.org
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Codec.Archive.Tar.Pack (
    pack,
    packFileEntry,
    packDirectoryEntry,

    getDirectoryContentsRecursive,
  ) where

import Codec.Archive.Tar.Types

import qualified Data.ByteString.Lazy as BS
import System.FilePath
         ( (</>) )
import qualified System.FilePath as FilePath.Native
         ( addTrailingPathSeparator, hasTrailingPathSeparator )
import System.Directory
         ( getDirectoryContents, doesDirectoryExist, getModificationTime
         , Permissions(..), getPermissions )
#if MIN_VERSION_directory(1,2,0)
-- The directory package switched to the new time package
import Data.Time.Clock
         ( UTCTime )
import Data.Time.Clock.POSIX
         ( utcTimeToPOSIXSeconds )
#else
import System.Time
         ( ClockTime(..) )
#endif
import System.IO
         ( IOMode(ReadMode), openBinaryFile, hFileSize )
import System.IO.Unsafe (unsafeInterleaveIO)

-- | Creates a tar archive from a list of directory or files. Any directories
-- specified will have their contents included recursively. Paths in the
-- archive will be relative to the given base directory.
--
-- This is a portable implementation of packing suitable for portable archives.
-- In particular it only constructs 'NormalFile' and 'Directory' entries. Hard
-- links and symbolic links are treated like ordinary files. It cannot be used
-- to pack directories containing recursive symbolic links. Special files like
-- FIFOs (named pipes), sockets or device files will also cause problems.
--
-- An exception will be thrown for any file names that are too long to
-- represent as a 'TarPath'.
--
-- * This function returns results lazily. Subdirectories are scanned
-- and files are read one by one as the list of entries is consumed.
--
pack :: FilePath   -- ^ Base directory
     -> [FilePath] -- ^ Files and directories to pack, relative to the base dir
     -> IO [Entry]
pack baseDir paths0 = preparePaths baseDir paths0 >>= packPaths baseDir

preparePaths :: FilePath -> [FilePath] -> IO [FilePath]
preparePaths baseDir paths =
  fmap concat $ interleave
    [ do isDir  <- doesDirectoryExist (baseDir </> path)
         if isDir
           then do entries <- getDirectoryContentsRecursive (baseDir </> path)
                   let entries' = map (path </>) entries
                       dir = FilePath.Native.addTrailingPathSeparator path
                   if null path then return entries'
                                else return (dir : entries')
           else return [path]
    | path <- paths ]

packPaths :: FilePath -> [FilePath] -> IO [Entry]
packPaths baseDir paths =
  interleave
    [ do tarpath <- either fail return (toTarPath isDir relpath)
         if isDir then packDirectoryEntry filepath tarpath
                  else packFileEntry      filepath tarpath
    | relpath <- paths
    , let isDir    = FilePath.Native.hasTrailingPathSeparator filepath
          filepath = baseDir </> relpath ]

interleave :: [IO a] -> IO [a]
interleave = unsafeInterleaveIO . go
  where
    go []     = return []
    go (x:xs) = do
      x'  <- x
      xs' <- interleave xs
      return (x':xs')

-- | Construct a tar 'Entry' based on a local file.
--
-- This sets the entry size, the data contained in the file and the file's
-- modification time. If the file is executable then that information is also
-- preserved. File ownership and detailed permissions are not preserved.
--
-- * The file contents is read lazily.
--
packFileEntry :: FilePath -- ^ Full path to find the file on the local disk
              -> TarPath  -- ^ Path to use for the tar Entry in the archive
              -> IO Entry
packFileEntry filepath tarpath = do
  mtime   <- getModTime filepath
  perms   <- getPermissions filepath
  file    <- openBinaryFile filepath ReadMode
  size    <- hFileSize file
  content <- BS.hGetContents file
  return (simpleEntry tarpath (NormalFile content (fromIntegral size))) {
    entryPermissions = if executable perms then executableFilePermissions
                                           else ordinaryFilePermissions,
    entryTime = mtime
  }

-- | Construct a tar 'Entry' based on a local directory (but not its contents).
--
-- The only attribute of the directory that is used is its modification time.
-- Directory ownership and detailed permissions are not preserved.
--
packDirectoryEntry :: FilePath -- ^ Full path to find the file on the local disk
                   -> TarPath  -- ^ Path to use for the tar Entry in the archive
                   -> IO Entry
packDirectoryEntry filepath tarpath = do
  mtime   <- getModTime filepath
  return (directoryEntry tarpath) {
    entryTime = mtime
  }

-- | This is a utility function, much like 'getDirectoryContents'. The
-- difference is that it includes the contents of subdirectories.
--
-- The paths returned are all relative to the top directory. Directory paths
-- are distinguishable by having a trailing path separator
-- (see 'FilePath.Native.hasTrailingPathSeparator').
--
-- All directories are listed before the files that they contain. Amongst the
-- contents of a directory, subdirectories are listed after normal files. The
-- overall result is that files within a directory will be together in a single
-- contiguous group. This tends to improve file layout and IO performance when
-- creating or extracting tar archives.
--
-- * This function returns results lazily. Subdirectories are not scanned
-- until the files entries in the parent directory have been consumed.
--
getDirectoryContentsRecursive :: FilePath -> IO [FilePath]
getDirectoryContentsRecursive dir0 =
  fmap tail (recurseDirectories dir0 [""])

recurseDirectories :: FilePath -> [FilePath] -> IO [FilePath]
recurseDirectories _    []         = return []
recurseDirectories base (dir:dirs) = unsafeInterleaveIO $ do
  (files, dirs') <- collect [] [] =<< getDirectoryContents (base </> dir)

  files' <- recurseDirectories base (dirs' ++ dirs)
  return (dir : files ++ files')

  where
    collect files dirs' []              = return (reverse files, reverse dirs')
    collect files dirs' (entry:entries) | ignore entry
                                        = collect files dirs' entries
    collect files dirs' (entry:entries) = do
      let dirEntry  = dir </> entry
          dirEntry' = FilePath.Native.addTrailingPathSeparator dirEntry
      isDirectory <- doesDirectoryExist (base </> dirEntry)
      if isDirectory
        then collect files (dirEntry':dirs') entries
        else collect (dirEntry:files) dirs' entries

    ignore ['.']      = True
    ignore ['.', '.'] = True
    ignore _          = False

getModTime :: FilePath -> IO EpochTime
getModTime path = do
#if MIN_VERSION_directory(1,2,0)
  -- The directory package switched to the new time package
  t <- getModificationTime path
  return . floor . utcTimeToPOSIXSeconds $ t
#else
  (TOD s _) <- getModificationTime path
  return $! fromIntegral s
#endif
