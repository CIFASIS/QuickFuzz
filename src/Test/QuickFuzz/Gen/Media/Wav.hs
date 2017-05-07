{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.QuickFuzz.Gen.Media.Wav where

import Data.Default

import qualified Data.Binary as B

import Sound.Wav

import Test.QuickCheck

import Test.QuickFuzz.Derive.Arbitrary
import Test.QuickFuzz.Derive.Show
import Test.QuickFuzz.Gen.FormatInfo
import Test.QuickFuzz.Gen.Base.ByteString

devArbitrary ''WaveFile

wavInfo :: FormatInfo WaveFile NoActions
wavInfo = def
    { encode = B.encode
    , random = arbitrary
    , ext = "wav"
    }
