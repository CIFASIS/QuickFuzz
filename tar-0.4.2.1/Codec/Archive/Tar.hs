-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Archive.Tar
-- Copyright   :  (c) 2007 Bjorn Bringert,
--                    2008 Andrea Vezzosi,
--                    2008-2012 Duncan Coutts
-- License     :  BSD3
--
-- Maintainer  :  duncan@community.haskell.org
-- Portability :  portable
--
-- Reading, writing and manipulating \"@.tar@\" archive files.
--
-- This module uses common names and so is designed to be imported qualified:
--
-- > import qualified Codec.Archive.Tar as Tar
--
-----------------------------------------------------------------------------
module Codec.Archive.Tar (

  -- | Tar archive files are used to store a collection of other files in a
  -- single file. They consists of a sequence of entries. Each entry describes
  -- a file or directory (or some other special kind of file). The entry stores
  -- a little bit of meta-data, in particular the file or directory name.
  --
  -- Unlike some other archive formats, a tar file contains no index. The
  -- information about each entry is stored next to the entry. Because of this,
  -- tar files are almost always processed linearly rather than in a
  -- random-access fashion.
  --
  -- The functions in this package are designed for working on tar files
  -- linearly and lazily. This makes it possible to do many operations in
  -- constant space rather than having to load the entire archive into memory.
  --
  -- It can read and write standard POSIX tar files and also the GNU and old
  -- Unix V7 tar formats. The convenience functions that are provided in the
  -- "Codec.Archive.Tar.Entry" module for creating archive entries are
  -- primarily designed for standard portable archives. If you need to
  -- construct GNU format archives or exactly preserve file ownership and
  -- permissions then you will need to write some extra helper functions.
  --
  -- This module contains just the simple high level operations without
  -- exposing the all the details of tar files. If you need to inspect tar
  -- entries in more detail or construct them directly then you also need
  -- the module "Codec.Archive.Tar.Entry".

  -- * High level \"all in one\" operations
  create,
  extract,
  append,

  -- * Notes
  -- ** Compressed tar archives
  -- | Tar files are commonly used in conjunction with gzip compression, as in
  -- \"@.tar.gz@\" or \"@.tar.bz2@\" files. This module does not directly
  -- handle compressed tar files however they can be handled easily by
  -- composing functions from this module and the modules
  -- @Codec.Compression.GZip@ or @Codec.Compression.BZip@
  -- (see @zlib@ or @bzlib@ packages).
  --
  -- Creating a compressed \"@.tar.gz@\" file is just a minor variation on the
  -- 'create' function, but where throw compression into the pipeline:
  --
  -- > BS.writeFile tar . GZip.compress . Tar.write =<< Tar.pack base dir
  --
  -- Similarly, extracting a compressed \"@.tar.gz@\" is just a minor variation
  -- on the 'extract' function where we use decompression in the pipeline:
  --
  -- > Tar.unpack dir . Tar.read . GZip.decompress =<< BS.readFile tar
  --

  -- ** Security
  -- | This is pretty important. A maliciously constructed tar archives could
  -- contain entries that specify bad file names. It could specify absolute
  -- file names like \"@\/etc\/passwd@\" or relative files outside of the
  -- archive like \"..\/..\/..\/something\". This security problem is commonly
  -- called a \"directory traversal vulnerability\". Historically, such
  -- vulnerabilities have been common in packages handling tar archives.
  --
  -- The 'extract' and 'unpack' functions check for bad file names. See the
  -- 'checkSecurity' function for more details. If you need to do any custom
  -- unpacking then you should use this.

  -- ** Tarbombs
  -- | A \"tarbomb\" is a @.tar@ file where not all entries are in a
  -- subdirectory but instead files extract into the top level directory. The
  -- 'extract' function does not check for these however if you want to do
  -- that you can use the 'checkTarbomb' function like so:
  --
  -- > Tar.unpack dir . Tar.checkTarbomb expectedDir
  -- >                . Tar.read =<< BS.readFile tar
  --
  -- In this case extraction will fail if any file is outside of @expectedDir@.

  -- * Converting between internal and external representation
  -- | Note, you cannot expect @write . read@ to give exactly the same output
  -- as input. You can expect the information to be preserved exactly however.
  -- This is because 'read' accepts common format variations while 'write'
  -- produces the standard format.
  read,
  write,

  -- * Packing and unpacking files to\/from internal representation
  -- | These functions are for packing and unpacking portable archives. They
  -- are not suitable in cases where it is important to preserve file ownership
  -- and permissions or to archive special files like named pipes and Unix
  -- device files.
  pack,
  unpack,

  -- * Types
  -- ** Tar entry type
  -- | This module provides only very simple and limited read-only access to
  -- the 'Entry' type. If you need access to the details or if you need to
  -- construct your own entries then also import "Codec.Archive.Tar.Entry".
  Entry,
  entryPath,
  entryContent,
  EntryContent(..),

  -- ** Sequences of tar entries
  Entries(..),
  mapEntries,
  mapEntriesNoFail,
  foldEntries,
  unfoldEntries,

  -- * Error handling
  -- | Reading tar files can fail if the data does not match the tar file
  -- format correctly.
  --
  -- The style of error handling by returning structured errors. The pure
  -- functions in the library do not throw exceptions, they return the errors
  -- as data. The IO actions in the library can throw exceptions, in particular
  -- the 'unpack' action does this. All the error types used are an instance of
  -- the standard 'Exception' class so it is possible to 'throw' and 'catch'
  -- them.

  -- ** Errors from reading tar files
  FormatError(..),
  ) where

import Codec.Archive.Tar.Types

import Codec.Archive.Tar.Read
import Codec.Archive.Tar.Write

import Codec.Archive.Tar.Pack
import Codec.Archive.Tar.Unpack
import Codec.Archive.Tar.Index (hSeekEndEntryOffset)

import Codec.Archive.Tar.Check

import Control.Exception (Exception, throw, catch)
import qualified Data.ByteString.Lazy as BS
import System.IO (withFile, IOMode(..))
import Prelude hiding (read)

-- | Create a new @\".tar\"@ file from a directory of files.
--
-- It is equivalent to calling the standard @tar@ program like so:
--
-- @$ tar -f tarball.tar -C base -c dir@
--
-- This assumes a directory @.\/base\/dir@ with files inside, eg
-- @.\/base\/dir\/foo.txt@. The file names inside the resulting tar file will be
-- relative to @dir@, eg @dir\/foo.txt@.
--
-- This is a high level \"all in one\" operation. Since you may need variations
-- on this function it is instructive to see how it is written. It is just:
--
-- > BS.writeFile tar . Tar.write =<< Tar.pack base paths
--
-- Notes:
--
-- The files and directories must not change during this operation or the
-- result is not well defined.
--
-- The intention of this function is to create tarballs that are portable
-- between systems. It is /not/ suitable for doing file system backups because
-- file ownership and permissions are not fully preserved. File ownership is
-- not preserved at all. File permissions are set to simple portable values:
--
-- * @rw-r--r--@ for normal files
--
-- * @rwxr-xr-x@ for executable files
--
-- * @rwxr-xr-x@ for directories
--
create :: FilePath   -- ^ Path of the \".tar\" file to write.
       -> FilePath   -- ^ Base directory
       -> [FilePath] -- ^ Files and directories to archive, relative to base dir
       -> IO ()
create tar base paths = BS.writeFile tar . write =<< pack base paths

-- | Extract all the files contained in a @\".tar\"@ file.
--
-- It is equivalent to calling the standard @tar@ program like so:
--
-- @$ tar -x -f tarball.tar -C dir@
--
-- So for example if the @tarball.tar@ file contains @foo\/bar.txt@ then this
-- will extract it to @dir\/foo\/bar.txt@.
--
-- This is a high level \"all in one\" operation. Since you may need variations
-- on this function it is instructive to see how it is written. It is just:
--
-- > Tar.unpack dir . Tar.read =<< BS.readFile tar
--
-- Notes:
--
-- Extracting can fail for a number of reasons. The tarball may be incorrectly
-- formatted. There may be IO or permission errors. In such cases an exception
-- will be thrown and extraction will not continue.
--
-- Since the extraction may fail part way through it is not atomic. For this
-- reason you may want to extract into an empty directory and, if the
-- extraction fails, recursively delete the directory.
--
-- Security: only files inside the target directory will be written. Tarballs
-- containing entries that point outside of the tarball (either absolute paths
-- or relative paths) will be caught and an exception will be thrown.
--
extract :: FilePath -- ^ Destination directory
        -> FilePath -- ^ Tarball
        -> IO ()
extract dir tar = unpack dir . read =<< BS.readFile tar

-- | Append new entries to a @\".tar\"@ file from a directory of files.
--
-- This is much like 'create', except that all the entries are added to the
-- end of an existing tar file. Or if the file does not already exists then
-- it behaves the same as 'create'.
--
append :: FilePath   -- ^ Path of the \".tar\" file to write.
       -> FilePath   -- ^ Base directory
       -> [FilePath] -- ^ Files and directories to archive, relative to base dir
       -> IO ()
append tar base paths =
    withFile tar ReadWriteMode $ \hnd -> do
      _ <- hSeekEndEntryOffset hnd Nothing
      BS.hPut hnd . write =<< pack base paths

