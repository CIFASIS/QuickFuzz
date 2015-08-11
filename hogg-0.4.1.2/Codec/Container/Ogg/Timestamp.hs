--
-- Module      : Timestamp
-- Copyright   : (c) Conrad Parker 2006
-- License     : BSD-style
-- Maintainer  : conradp@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable

module Codec.Container.Ogg.Timestamp (
  Timestamp (..),
  Timestampable,
  zeroTimestamp,
  timestampOf,
  between,
  before
) where

import Data.Char
import Data.Maybe
import Data.Ratio
import Text.Printf

import Codec.Container.Ogg.TimeScheme

-- Timestamp (n, d) is similar to Rational, but we ensure to keep the
-- original denominator around without simplifying, as it is derived from
-- the framerate / samplerate
data Timestamp =
  Timestamp {
    stamp :: (Integer, Integer)
  }
  -- deriving Ord, Eq

------------------------------------------------------------
-- Ord
--

instance Ord Timestamp where
  compare (Timestamp (n1,d1)) (Timestamp (n2,d2)) = compare (n1 % d1) (n2 % d2)

instance Eq Timestamp where
  (Timestamp (n1,d1)) == (Timestamp (n2,d2)) = (n1 % d1) == (n2 % d2)

------------------------------------------------------------
-- Constants
--

zeroTimestamp :: Timestamp
zeroTimestamp = Timestamp (0,1)

------------------------------------------------------------
-- Show
--

instance Show Timestamp where
  show (Timestamp (n,d))
    | d == 0    = "00:00:00.000"
    | d < 100   = printf "%02d:%02d:%02d::%02d" hrs minN secN framesN
    | otherwise = printf "%02d:%02d:%02d.%03d" hrs minN secN msN
    where
          msN = quot (1000 * framesN) d
          (secT, framesN) = quotRem n d
          (minT, secN) = quotRem secT 60
          (hrs, minN) = quotRem minT 60

------------------------------------------------------------
-- Read
--

data ParsedTimeStamp =
  ParsedTimeStamp {
    _hours :: Integer,
    _minutes :: Integer,
    _seconds :: Integer,
    _subseconds :: Either Integer Integer -- Left ms or Right frames
  }

instance Read Timestamp where
  readsPrec _ = readsTimestamp

readsTimestamp :: ReadS Timestamp
readsTimestamp str = [(t, rest) |
                      (scheme, r) <- reads str :: [(TimeScheme, String)],
                      (time, rest) <- readTime r,
                      t <- makeStamp scheme time]

makeStamp :: TimeScheme -> ParsedTimeStamp -> [Timestamp]
makeStamp scheme ts = map rToTs (timeSum rate ts)
  where
    rate = timeSchemeRate scheme
    rToTs x = Timestamp x

timeSum :: Rational -> ParsedTimeStamp -> [(Integer, Integer)]
timeSum rate (ParsedTimeStamp hh mm ss subs) = case subs of
    Left ms -> [((t 1000 1 ms) , 1000)]
    Right ff -> [((t n d ff) , n)]
  where
      n = numerator rate
      d = denominator rate
      t tn td z = ((hh*60 +mm)*60 +ss)*tn + td*z

readTime :: String -> [(ParsedTimeStamp, String)]
readTime str = maybe [] (\x -> [(x, rest)]) parsed
  where 
        (t, rest) = span (\x -> isAlphaNum x || x == ':' || x == '.') str
        flam = split ':' t
        parsed :: Maybe ParsedTimeStamp
        parsed = case flam of
          [hh, mm, ss, "", ff] -> fromFrames hh mm ss ff
          [mm, ss, "", ff] -> fromFrames "00" mm ss ff
          [ss, "", ff] -> fromFrames "00" "00" ss ff
          ["", ff] -> fromFrames "00" "00" "00" ff
          [hh, mm, ss] -> fromNPT hh mm ss
          [mm, ss] -> fromNPT "00" mm ss
          [ss] -> fromNPT "00" "00" ss
          _ -> Nothing

        fromFrames :: String -> String -> String -> String
                   -> Maybe ParsedTimeStamp
        fromFrames hh mm ss ff = do
          h <- twoDigits hh
          m <- twoDigits mm
          s <- twoDigits ss
          f <- twoDigits ff
          return $ ParsedTimeStamp h m s (Right f)

        fromNPT hh mm ss = do
          h <- twoDigits hh
          m <- twoDigits mm
          (s, ms) <- fDigits ss
          return $ ParsedTimeStamp h m s (Left ms)

        fDigits a = do
          let (ss:mss) = split '.' a
          s <- twoDigits ss
          ms <- threeDigits $ safeHead mss
          let r = (s, ms)
          return r

        dig = fromIntegral . digitToInt

        safeHead [] = []
        safeHead (x:_) = x

        twoDigits [a,b] = Just (10 * (dig a) + dig b)
        twoDigits [a] = Just (dig a)
        twoDigits _ = Nothing

        threeDigits (a:b:c:_) =
          Just (100 * (dig a) + 10 * (dig b) + dig c)
        threeDigits [a,b] = Just (100 * (dig a) + 10 * dig b)
        threeDigits [a] = Just (100 * dig a)
        threeDigits [] = Just 0

split :: Eq a => a -> [a] -> [[a]]
split delim s
  | rest == [] = [token]
  | otherwise  = token : split delim (tail rest)
  where (token, rest) = span (/= delim) s

------------------------------------------------------------
-- Timestampable
--

class Timestampable a where
  timestampOf :: a -> Maybe Timestamp

between :: (Timestampable a) => Maybe Timestamp -> Maybe Timestamp -> [a] -> [a]
between start end xs = case start of
  Nothing -> takeWhile (before end) xs
  _       -> takeWhile (before end) (dropWhile (before start) xs)

before :: (Timestampable a) => Maybe Timestamp -> a -> Bool
before Nothing _ = True
before (Just b) x = t == Nothing || (fromJust t) <= b
  where
    t = timestampOf x

