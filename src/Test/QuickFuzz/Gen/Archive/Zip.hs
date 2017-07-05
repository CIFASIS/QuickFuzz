{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.QuickFuzz.Gen.Archive.Zip (
zipInfo
) where

import Data.Default

import Control.Exception
import Control.DeepSeq
import Data.ByteString.Lazy
import Test.QuickCheck
import Data.Word

import Codec.Archive.Zip

import Test.QuickFuzz.Derive.Arbitrary
import Test.QuickFuzz.Derive.Actions
import Test.QuickFuzz.Derive.Show
import Test.QuickFuzz.Derive.NFData
import Test.QuickFuzz.Gen.FormatInfo
import Test.QuickFuzz.Gen.Base.ByteString
import Test.QuickFuzz.Gen.Base.String

$(devActions ["Codec.Archive.Zip"] ''Entry False [] [])
$(devArbitrary ''EntryAction)
$(devArbitraryWithActions False ''Entry)

$(devShow ''EntryAction)

$(devActions ["Codec.Archive.Zip"] ''Archive False [] [])
$(devArbitrary ''ArchiveAction)
$(devArbitraryWithActions False ''Archive )

$(devShow ''ArchiveAction)

$(devNFData ''Archive)

zipInfo :: FormatInfo Archive ArchiveAction
zipInfo = def 
    { encode = fromArchive
    , random = arbitrary
    , value = show
    , actions = Just $ def 
        { randomActions = arbitrary
        , shrinkActions = shrinkArchiveAction
        , performActions = performArchiveAction }
    , ext = "zip"
    } 
