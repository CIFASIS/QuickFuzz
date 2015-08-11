--
-- Module      : MessageHeaders
-- Copyright   : (c) Conrad Parker 2006
-- License     : BSD-style
-- Maintainer  : conradp@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable

module Codec.Container.Ogg.MessageHeaders (
  MessageHeaders(..),
  mhEmpty,
  mhSingleton,
  mhInsert,
  mhAppend
) where

import Data.Char
import Data.List as List
import qualified Data.Map as Map

------------------------------------------------------------
-- Data
--

data MessageHeaders =
  MessageHeaders {
    mhHeaders :: Map.Map String [String]
  }
    
------------------------------------------------------------
-- Constructors
--

mhEmpty :: MessageHeaders
mhEmpty = MessageHeaders (Map.empty)

mhSingleton :: String -> String -> MessageHeaders
mhSingleton f v = MessageHeaders (Map.singleton f [v])

------------------------------------------------------------
-- Insert
--

mhInsert :: String -> String -> MessageHeaders -> MessageHeaders
mhInsert k v (MessageHeaders h) = MessageHeaders (Map.insert k [v] h)

------------------------------------------------------------
-- Append
--

mhAppend :: String -> String -> MessageHeaders -> MessageHeaders
mhAppend k v mhdrs = mhAppends k [v] mhdrs

mhAppends :: String -> [String] -> MessageHeaders -> MessageHeaders
mhAppends k vs (MessageHeaders h) = MessageHeaders (Map.insertWith (++) k vs h)

------------------------------------------------------------
-- Read
--

instance Read MessageHeaders where
  readsPrec _ = readMH mhEmpty

readMH :: MessageHeaders -> ReadS MessageHeaders
readMH mhdrs s = readMHlines mhdrs (lines s)

readMHlines :: MessageHeaders -> [String] -> [(MessageHeaders, String)]
readMHlines mhdrs [] = [(mhdrs, "")]

-- Stop parsing at an empty line, ie. when the input contains CRLFCRLF
readMHlines mhdrs ("":body) = [(mhdrs, unlines body)]

readMHlines mhdrs (l:rest) = readMHlines mhdrs' rest
  where
    mhdrs' = mhAppends k vs mhdrs
    (k, ':':vs'csv) = break (':' ==) l
    vs' = split ',' vs'csv
    vs = filter (not . null) $ map (filter isHTTPTokenChar) vs'

split :: Char -> String -> [String]
split = unfoldr . split'

split' :: Char -> String -> Maybe (String, String)
split' c l
  | null l = Nothing
  | otherwise = Just (h, drop 1 t)
  where (h, t) = span (/=c) l

------------------------------------------------------------
-- Read helpers, based on grammar of RFC2616 sec. 2.1
-- http://www.w3.org/Protocols/rfc2616/rfc2616-sec2.html#sec2
--

isHTTPCTL :: Char -> Bool
isHTTPCTL c = (ord c <= 31) || (ord c) == 127

isHTTPSeparator :: Char -> Bool
isHTTPSeparator = flip elem ['(' , ')' , '<' , '>' , '@'
                            , ',' , ';' , ':' , '\\' , '\"'
                            , '/' , '[' , ']' , '?' , '='
                            , '{' , '}' , ' ', '\t']

isHTTPTokenChar :: Char -> Bool
isHTTPTokenChar c = not (isHTTPCTL c || isHTTPSeparator c)

------------------------------------------------------------
-- Show
--

instance Show MessageHeaders where
  show (MessageHeaders h) =
    concat $ List.map serializeMH (Map.assocs h)
    where
      serializeMH :: (String, [String]) -> String
      serializeMH (k, v) = k ++ ": " ++ (concat $ intersperse ", " v) ++ "\r\n"
