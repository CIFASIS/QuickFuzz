{-# LANGUAGE FlexibleInstances, IncoherentInstances #-}
module Unicode where

import Test.QuickCheck.Unicode

import qualified Data.ByteString.Lazy.Char8 as C8

mencode :: Unicode String -> C8.ByteString
mencode = C8.pack . fromUnicode
