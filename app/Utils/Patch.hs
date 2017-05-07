module Utils.Patch where

import Prelude hiding (concat)

import Data.ByteString.Lazy (ByteString, pack, unpack, concat, toStrict)

import Data.String
import Data.Algorithm.Diff
import Data.ByteString.Search


type Patch = (ByteString, ByteString, ByteString, ByteString)

diff :: ByteString -> ByteString -> [Patch]
diff old new = groupDiffs (getGroupedDiff (unpack old) (unpack new))

groupDiffs (Both pre _ : First old : Second new : Both pos _ : xs) =
    (pack pre, pack pos, pack (take (length new) old), pack new) : groupDiffs xs
groupDiffs (x:xs) = groupDiffs xs
groupDiffs _ = []


patch :: [Patch] -> ByteString -> ByteString
patch [] orig = orig
patch (p:ps) orig = patch ps (applyPatch p orig)

applyPatch :: Patch -> ByteString -> ByteString
applyPatch (pre, pos, old, new) orig = 
    replace (toStrict (concat [pre,old,pos])) 
            (concat [pre,new,pos]) 
            (toStrict orig) 
