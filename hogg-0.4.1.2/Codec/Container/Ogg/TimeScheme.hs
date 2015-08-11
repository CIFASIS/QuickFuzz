--
-- Module      : TimeScheme
-- Copyright   : (c) Conrad Parker 2006
-- License     : BSD-style
-- Maintainer  : conradp@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable

module Codec.Container.Ogg.TimeScheme (
  TimeScheme (..),
  guessTimeScheme,

  -- | Some standard time schemes
  npt,
  smpte24,
  smpte24drop,
  smpte25,
  smpte25drop,
  smpte30,
  smpte30drop,
  smpte50,
  smpte60,
  smpte60drop
) where

import Data.Char
import Data.Maybe
import Data.Ratio

------------------------------------------------------------
-- TimeScheme
--

data TimeScheme =
  TimeScheme {
    timeSchemeName :: String,
    timeSchemeRate :: Rational
  }

-- | Standard TimeSchemes
knownTimeSchemes :: [TimeScheme]
knownTimeSchemes = [npt, smpte24, smpte24drop, smpte25, smpte25drop,
                    smpte30, smpte30drop, smpte50, smpte60, smpte60drop]

-- | Guess the TimeScheme by rate
guessTimeScheme :: Rational -> Maybe TimeScheme
guessTimeScheme r = listToMaybe $ filter sameRate knownTimeSchemes
  where
    sameRate = \x ->  timeSchemeRate x == r

------------------------------------------------------------
-- Read
--

instance Read TimeScheme where
  readsPrec _ = readsTimeScheme

readsTimeScheme :: ReadS TimeScheme
readsTimeScheme [] = []
readsTimeScheme str@(c:_)
  | isDigit c = [(npt, str)]
  | otherwise = [(scheme, tail rest) | scheme <- matches]
  where (tok, rest) = span (\x -> isAlphaNum x || x == '-') str
        matches = filter sameName knownTimeSchemes
        sameName = \x ->  l (timeSchemeName x) == l tok
        l = map toLower

------------------------------------------------------------
-- Show
--

instance Show TimeScheme where
  show = timeSchemeName

------------------------------------------------------------
-- Known TimeSchemes
--

npt :: TimeScheme
npt = TimeScheme "npt" (1000%1)

smpte24 :: TimeScheme
smpte24 = TimeScheme "smpte-24" (24%1)

smpte24drop :: TimeScheme
smpte24drop = TimeScheme "smpte-24-drop" (24000%1001)

smpte25 :: TimeScheme
smpte25 = TimeScheme "smpte-25" (25%1)

smpte25drop :: TimeScheme
smpte25drop = TimeScheme "smpte-25-drop" (25000%1001)

smpte30 :: TimeScheme
smpte30 = TimeScheme "smpte-30" (30%1)

smpte30drop :: TimeScheme
smpte30drop = TimeScheme "smpte-30-drop" (30000%1001)

smpte50 :: TimeScheme
smpte50 = TimeScheme "smpte-50" (50%1)

smpte60 :: TimeScheme
smpte60 = TimeScheme "smpte-60" (60%1)

smpte60drop :: TimeScheme
smpte60drop = TimeScheme "smpte-60" (60000%1001)
