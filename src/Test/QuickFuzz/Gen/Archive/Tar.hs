{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.QuickFuzz.Gen.Archive.Tar where

import Data.Default

import Control.Exception
import Data.ByteString.Lazy
import Test.QuickCheck

import Codec.Archive.Tar
import Codec.Archive.Tar.Entry
-- Dirty hack to put Codec.Archive.Tar.Types in scope, since it is
-- hidden, but reexported in Codec.Archive.Tar.Entry
import Codec.Archive.Tar.Entry as Codec.Archive.Tar.Types

import Test.QuickFuzz.Derive.Arbitrary
import Test.QuickFuzz.Derive.Actions
import Test.QuickFuzz.Derive.Show
import Test.QuickFuzz.Gen.FormatInfo
import Test.QuickFuzz.Gen.Base.ByteString

devActions ["Codec.Archive.Tar.Entry"] ''Entry False [''Entry] []
devArbitrary ''EntryAction
devArbitraryWithActions False ''Entry 
devShow ''Entry
devShow ''EntryAction

tarInfo :: FormatInfo [Entry] EntryAction
tarInfo = def 
    { encode = write
    , random = arbitrary
    , value = show 
    , actions = Just $ def 
        { randomActions = arbitrary
        , shrinkActions = shrinkEntryAction
        , performActions = pure . performEntryAction } 
    , ext = "tar"
    } 
