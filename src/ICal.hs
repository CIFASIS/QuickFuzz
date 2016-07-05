
{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module ICal where

import Test.QuickCheck
import Test.QuickCheck.Instances
import DeriveArbitrary
import Text.ICalendar.Types
import Text.ICalendar.Printer
import Data.Char (ord)

--import Time
import qualified Data.ByteString.Lazy.Char8 as LC8

import qualified Data.ByteString.Lazy.Builder as Bu

$(devArbitrary ''VCalendar)

utf8Len :: Char -> Int
utf8Len c | o < 0x80  = 1
          | o < 0x800  = 2
          | o < 0x10000 = 3
          | o < 0x200000 = 4
          | o < 0x4000000 = 5
          | otherwise      = 6
  where o = ord c

mencode :: VCalendar -> LC8.ByteString
mencode x = printICalendar (EncodingFunctions Bu.charUtf8 utf8Len) x
