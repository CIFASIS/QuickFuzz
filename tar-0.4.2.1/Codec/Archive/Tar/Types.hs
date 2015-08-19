-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Archive.Tar.Types
-- Copyright   :  (c) 2007 Bjorn Bringert,
--                    2008 Andrea Vezzosi,
--                    2008-2009 Duncan Coutts
--                    2011 Max Bolingbroke
-- License     :  BSD3
--
-- Maintainer  :  duncan@community.haskell.org
-- Portability :  portable
--
-- Types to represent the content of @.tar@ archives.
--
-----------------------------------------------------------------------------
module Codec.Archive.Tar.Types (

  Entry(..),
  entryPath,
  EntryContent(..),
  FileSize,
  Permissions,
  Ownership(..),
  EpochTime,
  TypeCode,
  DevMajor,
  DevMinor,
  Format(..),

  simpleEntry,
  fileEntry,
  directoryEntry,

  ordinaryFilePermissions,
  executableFilePermissions,
  directoryPermissions,

  TarPath(..),
  toTarPath,
  fromTarPath,
  fromTarPathToPosixPath,
  fromTarPathToWindowsPath,

  LinkTarget(..),
  toLinkTarget,
  fromLinkTarget,
  fromLinkTargetToPosixPath,
  fromLinkTargetToWindowsPath,

  Entries(..),
  mapEntries,
  mapEntriesNoFail,
  foldEntries,
  unfoldEntries,

  ) where

import Data.Int      (Int64)
import Data.Monoid   (Monoid(..))
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)

import qualified System.FilePath as FilePath.Native
         ( joinPath, splitDirectories, addTrailingPathSeparator )
import qualified System.FilePath.Posix as FilePath.Posix
         ( joinPath, splitPath, splitDirectories, hasTrailingPathSeparator
         , addTrailingPathSeparator )
import qualified System.FilePath.Windows as FilePath.Windows
         ( joinPath, addTrailingPathSeparator )
import System.Posix.Types
         ( FileMode )

type FileSize  = Int64
-- | The number of seconds since the UNIX epoch
type EpochTime = Int64
type DevMajor  = Int
type DevMinor  = Int
type TypeCode  = Char
type Permissions = FileMode

-- | Tar archive entry.
--
data Entry = Entry {

    -- | The path of the file or directory within the archive. This is in a
    -- tar-specific form. Use 'entryPath' to get a native 'FilePath'.
    entryTarPath :: !TarPath,

    -- | The real content of the entry. For 'NormalFile' this includes the
    -- file data. An entry usually contains a 'NormalFile' or a 'Directory'.
    entryContent :: !EntryContent,

    -- | File permissions (Unix style file mode).
    entryPermissions :: !Permissions,

    -- | The user and group to which this file belongs.
    entryOwnership :: !Ownership,

    -- | The time the file was last modified.
    entryTime :: !EpochTime,

    -- | The tar format the archive is using.
    entryFormat :: !Format
  }

-- | Native 'FilePath' of the file or directory within the archive.
--
entryPath :: Entry -> FilePath
entryPath = fromTarPath . entryTarPath

-- | The content of a tar archive entry, which depends on the type of entry.
--
-- Portable archives should contain only 'NormalFile' and 'Directory'.
--
data EntryContent = NormalFile      ByteString !FileSize
                  | Directory
                  | SymbolicLink    !LinkTarget
                  | HardLink        !LinkTarget
                  | CharacterDevice !DevMajor !DevMinor
                  | BlockDevice     !DevMajor !DevMinor
                  | NamedPipe
                  | OtherEntryType  !TypeCode ByteString !FileSize
    deriving (Eq, Ord)

data Ownership = Ownership {
    -- | The owner user name. Should be set to @\"\"@ if unknown.
    ownerName :: String,

    -- | The owner group name. Should be set to @\"\"@ if unknown.
    groupName :: String,

    -- | Numeric owner user id. Should be set to @0@ if unknown.
    ownerId :: !Int,

    -- | Numeric owner group id. Should be set to @0@ if unknown.
    groupId :: !Int
  }
    deriving (Eq, Ord)

-- | There have been a number of extensions to the tar file format over the
-- years. They all share the basic entry fields and put more meta-data in
-- different extended headers.
--
data Format =

     -- | This is the classic Unix V7 tar format. It does not support owner and
     -- group names, just numeric Ids. It also does not support device numbers.
     V7Format

     -- | The \"USTAR\" format is an extension of the classic V7 format. It was
     -- later standardised by POSIX. It has some restrictions but is the most
     -- portable format.
     --
   | UstarFormat

     -- | The GNU tar implementation also extends the classic V7 format, though
     -- in a slightly different way from the USTAR format. In general for new
     -- archives the standard USTAR/POSIX should be used.
     --
   | GnuFormat
  deriving (Eq, Ord)

-- | @rw-r--r--@ for normal files
ordinaryFilePermissions :: Permissions
ordinaryFilePermissions   = 0o0644

-- | @rwxr-xr-x@ for executable files
executableFilePermissions :: Permissions
executableFilePermissions = 0o0755

-- | @rwxr-xr-x@ for directories
directoryPermissions :: Permissions
directoryPermissions  = 0o0755

-- | An 'Entry' with all default values except for the file name and type. It
-- uses the portable USTAR/POSIX format (see 'UstarHeader').
--
-- You can use this as a basis and override specific fields, eg:
--
-- > (emptyEntry name HardLink) { linkTarget = target }
--
simpleEntry :: TarPath -> EntryContent -> Entry
simpleEntry tarpath content = Entry {
    entryTarPath     = tarpath,
    entryContent     = content,
    entryPermissions = case content of
                         Directory -> directoryPermissions
                         _         -> ordinaryFilePermissions,
    entryOwnership   = Ownership "" "" 0 0,
    entryTime        = 0,
    entryFormat      = UstarFormat
  }

-- | A tar 'Entry' for a file.
--
-- Entry  fields such as file permissions and ownership have default values.
--
-- You can use this as a basis and override specific fields. For example if you
-- need an executable file you could use:
--
-- > (fileEntry name content) { fileMode = executableFileMode }
--
fileEntry :: TarPath -> ByteString -> Entry
fileEntry name fileContent =
  simpleEntry name (NormalFile fileContent (BS.length fileContent))

-- | A tar 'Entry' for a directory.
--
-- Entry fields such as file permissions and ownership have default values.
--
directoryEntry :: TarPath -> Entry
directoryEntry name = simpleEntry name Directory

--
-- * Tar paths
--

-- | The classic tar format allowed just 100 characters for the file name. The
-- USTAR format extended this with an extra 155 characters, however it uses a
-- complex method of splitting the name between the two sections.
--
-- Instead of just putting any overflow into the extended area, it uses the
-- extended area as a prefix. The aggravating insane bit however is that the
-- prefix (if any) must only contain a directory prefix. That is the split
-- between the two areas must be on a directory separator boundary. So there is
-- no simple calculation to work out if a file name is too long. Instead we
-- have to try to find a valid split that makes the name fit in the two areas.
--
-- The rationale presumably was to make it a bit more compatible with old tar
-- programs that only understand the classic format. A classic tar would be
-- able to extract the file name and possibly some dir prefix, but not the
-- full dir prefix. So the files would end up in the wrong place, but that's
-- probably better than ending up with the wrong names too.
--
-- So it's understandable but rather annoying.
--
-- * Tar paths use Posix format (ie @\'/\'@ directory separators), irrespective
--   of the local path conventions.
--
-- * The directory separator between the prefix and name is /not/ stored.
--
data TarPath = TarPath FilePath -- path name, 100 characters max.
                       FilePath -- path prefix, 155 characters max.
  deriving (Eq, Ord)

-- | Convert a 'TarPath' to a native 'FilePath'.
--
-- The native 'FilePath' will use the native directory separator but it is not
-- otherwise checked for validity or sanity. In particular:
--
-- * The tar path may be invalid as a native path, eg the file name @\"nul\"@
--   is not valid on Windows.
--
-- * The tar path may be an absolute path or may contain @\"..\"@ components.
--   For security reasons this should not usually be allowed, but it is your
--   responsibility to check for these conditions (eg using 'checkSecurity').
--
fromTarPath :: TarPath -> FilePath
fromTarPath (TarPath name prefix) = adjustDirectory $
  FilePath.Native.joinPath $ FilePath.Posix.splitDirectories prefix
                          ++ FilePath.Posix.splitDirectories name
  where
    adjustDirectory | FilePath.Posix.hasTrailingPathSeparator name
                    = FilePath.Native.addTrailingPathSeparator
                    | otherwise = id

-- | Convert a 'TarPath' to a Unix/Posix 'FilePath'.
--
-- The difference compared to 'fromTarPath' is that it always returns a Unix
-- style path irrespective of the current operating system.
--
-- This is useful to check how a 'TarPath' would be interpreted on a specific
-- operating system, eg to perform portability checks.
--
fromTarPathToPosixPath :: TarPath -> FilePath
fromTarPathToPosixPath (TarPath name prefix) = adjustDirectory $
  FilePath.Posix.joinPath $ FilePath.Posix.splitDirectories prefix
                         ++ FilePath.Posix.splitDirectories name
  where
    adjustDirectory | FilePath.Posix.hasTrailingPathSeparator name
                    = FilePath.Posix.addTrailingPathSeparator
                    | otherwise = id

-- | Convert a 'TarPath' to a Windows 'FilePath'.
--
-- The only difference compared to 'fromTarPath' is that it always returns a
-- Windows style path irrespective of the current operating system.
--
-- This is useful to check how a 'TarPath' would be interpreted on a specific
-- operating system, eg to perform portability checks.
--
fromTarPathToWindowsPath :: TarPath -> FilePath
fromTarPathToWindowsPath (TarPath name prefix) = adjustDirectory $
  FilePath.Windows.joinPath $ FilePath.Posix.splitDirectories prefix
                           ++ FilePath.Posix.splitDirectories name
  where
    adjustDirectory | FilePath.Posix.hasTrailingPathSeparator name
                    = FilePath.Windows.addTrailingPathSeparator
                    | otherwise = id

-- | Convert a native 'FilePath' to a 'TarPath'.
--
-- The conversion may fail if the 'FilePath' is too long. See 'TarPath' for a
-- description of the problem with splitting long 'FilePath's.
--
toTarPath :: Bool -- ^ Is the path for a directory? This is needed because for
                  -- directories a 'TarPath' must always use a trailing @\/@.
          -> FilePath -> Either String TarPath
toTarPath isDir = splitLongPath
                . addTrailingSep
                . FilePath.Posix.joinPath
                . FilePath.Native.splitDirectories
  where
    addTrailingSep | isDir     = FilePath.Posix.addTrailingPathSeparator
                   | otherwise = id

-- | Take a sanitised path, split on directory separators and try to pack it
-- into the 155 + 100 tar file name format.
--
-- The strategy is this: take the name-directory components in reverse order
-- and try to fit as many components into the 100 long name area as possible.
-- If all the remaining components fit in the 155 name area then we win.
--
splitLongPath :: FilePath -> Either String TarPath
splitLongPath path =
  case packName nameMax (reverse (FilePath.Posix.splitPath path)) of
    Left err                 -> Left err
    Right (name, [])         -> Right (TarPath name "")
    Right (name, first:rest) -> case packName prefixMax remainder of
      Left err               -> Left err
      Right (_     , (_:_))  -> Left "File name too long (cannot split)"
      Right (prefix, [])     -> Right (TarPath name prefix)
      where
        -- drop the '/' between the name and prefix:
        remainder = init first : rest

  where
    nameMax, prefixMax :: Int
    nameMax   = 100
    prefixMax = 155

    packName _      []     = Left "File name empty"
    packName maxLen (c:cs)
      | n > maxLen         = Left "File name too long"
      | otherwise          = Right (packName' maxLen n [c] cs)
      where n = length c

    packName' maxLen n ok (c:cs)
      | n' <= maxLen             = packName' maxLen n' (c:ok) cs
                                     where n' = n + length c
    packName' _      _ ok    cs  = (FilePath.Posix.joinPath ok, cs)

-- | The tar format allows just 100 ASCII characters for the 'SymbolicLink' and
-- 'HardLink' entry types.
--
newtype LinkTarget = LinkTarget FilePath
  deriving (Eq, Ord)

-- | Convert a native 'FilePath' to a tar 'LinkTarget'. This may fail if the
-- string is longer than 100 characters or if it contains non-portable
-- characters.
--
toLinkTarget   :: FilePath -> Maybe LinkTarget
toLinkTarget path | length path <= 100 = Just (LinkTarget path)
                  | otherwise          = Nothing

-- | Convert a tar 'LinkTarget' to a native 'FilePath'.
--
fromLinkTarget :: LinkTarget -> FilePath
fromLinkTarget (LinkTarget path) = adjustDirectory $
  FilePath.Native.joinPath $ FilePath.Posix.splitDirectories path
  where
    adjustDirectory | FilePath.Posix.hasTrailingPathSeparator path
                    = FilePath.Native.addTrailingPathSeparator
                    | otherwise = id

-- | Convert a tar 'LinkTarget' to a Unix/Posix 'FilePath'.
--
fromLinkTargetToPosixPath :: LinkTarget -> FilePath
fromLinkTargetToPosixPath (LinkTarget path) = adjustDirectory $
  FilePath.Posix.joinPath $ FilePath.Posix.splitDirectories path
  where
    adjustDirectory | FilePath.Posix.hasTrailingPathSeparator path
                    = FilePath.Native.addTrailingPathSeparator
                    | otherwise = id

-- | Convert a tar 'LinkTarget' to a Windows 'FilePath'.
--
fromLinkTargetToWindowsPath :: LinkTarget -> FilePath
fromLinkTargetToWindowsPath (LinkTarget path) = adjustDirectory $
  FilePath.Windows.joinPath $ FilePath.Posix.splitDirectories path
  where
    adjustDirectory | FilePath.Posix.hasTrailingPathSeparator path
                    = FilePath.Windows.addTrailingPathSeparator
                    | otherwise = id

--
-- * Entries type
--

-- | A tar archive is a sequence of entries.
--
-- The point of this type as opposed to just using a list is that it makes the
-- failure case explicit. We need this because the sequence of entries we get
-- from reading a tarball can include errors.
--
-- It is a concrete data type so you can manipulate it directly but it is often
-- clearer to use the provided functions for mapping, folding and unfolding.
--
-- Converting from a list can be done with just @foldr Next Done@. Converting
-- back into a list can be done with 'foldEntries' however in that case you
-- must be prepared to handle the 'Fail' case inherent in the 'Entries' type.
--
-- The 'Monoid' instance lets you concatenate archives or append entries to an
-- archive.
--
data Entries e = Next Entry (Entries e)
               | Done
               | Fail e

infixr 5 `Next`

-- | This is like the standard 'unfoldr' function on lists, but for 'Entries'.
-- It includes failure as an extra possibility that the stepper function may
-- return.
--
-- It can be used to generate 'Entries' from some other type. For example it is
-- used internally to lazily unfold entries from a 'ByteString'.
--
unfoldEntries :: (a -> Either e (Maybe (Entry, a))) -> a -> Entries e
unfoldEntries f = unfold
  where
    unfold x = case f x of
      Left err             -> Fail err
      Right Nothing        -> Done
      Right (Just (e, x')) -> Next e (unfold x')

-- | This is like the standard 'foldr' function on lists, but for 'Entries'.
-- Compared to 'foldr' it takes an extra function to account for the
-- possibility of failure.
--
-- This is used to consume a sequence of entries. For example it could be used
-- to scan a tarball for problems or to collect an index of the contents.
--
foldEntries :: (Entry -> a -> a) -> a -> (e -> a) -> Entries e -> a
foldEntries next done fail' = fold
  where
    fold (Next e es) = next e (fold es)
    fold Done        = done
    fold (Fail err)  = fail' err

-- | This is like the standard 'map' function on lists, but for 'Entries'. It
-- includes failure as a extra possible outcome of the mapping function.
--
-- If your mapping function cannot fail it may be more convenient to use
-- 'mapEntriesNoFail'
mapEntries :: (Entry -> Either e' Entry) -> Entries e -> Entries (Either e e')
mapEntries f =
  foldEntries (\entry rest -> either (Fail . Right) (flip Next rest) (f entry)) Done (Fail . Left)

-- | Like 'mapEntries' but the mapping function itself cannot fail.
--
mapEntriesNoFail :: (Entry -> Entry) -> Entries e -> Entries e
mapEntriesNoFail f =
  foldEntries (\entry -> Next (f entry)) Done Fail

instance Monoid (Entries e) where
  mempty      = Done
  mappend a b = foldEntries Next b Fail a

instance Functor Entries where
  fmap f = foldEntries Next Done (Fail . f)

