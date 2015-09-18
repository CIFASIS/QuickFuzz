{-# OPTIONS  #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.Python.Common.StringEscape
-- Copyright   : (c) 2009 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : bjpop@csse.unimelb.edu.au
-- Stability   : experimental
-- Portability : ghc
--
-- Conversion to/from escaped characters in strings. Note: currently does not
-- support escaped Unicode character names.
-- 
-- See:
-- 
--    * Version 2.6 <http://www.python.org/doc/2.6/reference/lexical_analysis.html#string-literals>
--  
--    * Version 3.1 <http://www.python.org/doc/3.1/reference/lexical_analysis.html#string-and-bytes-literals> 
-----------------------------------------------------------------------------

module Language.JavaScript.Parser.StringEscape ( 
   -- * String conversion. 
   unescapeString, 
   unescapeRawString,
   -- * Digits allowed in octal and hex representation.
   octalDigits,
   hexDigits) where

import Numeric (readHex, readOct)

-- | Convert escaped sequences of characters into /real/ characters in a normal Python string.
unescapeString :: String -> String
unescapeString ('\\':'\\':cs) = '\\' : unescapeString cs -- Backslash (\)
unescapeString ('\\':'\'':cs) = '\'' : unescapeString cs -- Single quote (')
unescapeString ('\\':'"':cs) = '"' : unescapeString cs   -- Double quote (")
unescapeString ('\\':'a':cs) = '\a' : unescapeString cs  -- ASCII Bell (BEL)
unescapeString ('\\':'b':cs) = '\b' : unescapeString cs  -- ASCII Backspace (BS)
unescapeString ('\\':'f':cs) = '\f' : unescapeString cs  -- ASCII Formfeed (FF)
unescapeString ('\\':'n':cs) = '\n' : unescapeString cs  -- ASCII Linefeed (LF)
unescapeString ('\\':'r':cs) = '\r' : unescapeString cs  -- ASCII Carriage Return (CR)
unescapeString ('\\':'t':cs) = '\t' : unescapeString cs  -- ASCII Horizontal Tab (TAB)
unescapeString ('\\':'v':cs) = '\v' : unescapeString cs  -- ASCII Vertical Tab (VT)
unescapeString ('\\':'\n':cs) = unescapeString cs        -- line continuation
unescapeString ('\\':rest@(o:_))
   | o `elem` octalDigits = unescapeNumeric 3 octalDigits (fst . head . readOct) rest 
unescapeString ('\\':'x':rest@(h:_))
   | h `elem` hexDigits = unescapeNumeric 2 hexDigits (fst . head . readHex) rest 
unescapeString (c:cs) = c : unescapeString cs 
unescapeString [] = []

-- | Convert escaped sequences of characters into /real/ characters in a raw Python string.
-- Note: despite their name, Python raw strings do allow a small set of character escapings,
-- namely the single and double quote characters and the line continuation marker.
unescapeRawString :: String -> String
unescapeRawString ('\\':'\'':cs) = '\'' : unescapeRawString cs -- Single quote (')
unescapeRawString ('\\':'"':cs) = '"' : unescapeRawString cs -- Double quote (")
unescapeRawString ('\\':'\n':cs) = unescapeRawString cs -- line continuation
unescapeRawString (c:cs) = c : unescapeRawString cs
unescapeRawString [] = []

{- 
   This is a bit complicated because Python allows between 1 and 3 octal
   characters after the \, and 1 and 2 hex characters after a \x.
-}
unescapeNumeric :: Int -> String -> (String -> Int) -> String -> String
unescapeNumeric n numericDigits readNumeric str
   = loop n [] str 
   where
   loop _ acc [] = [numericToChar acc]
   loop 0 acc rest
      = numericToChar acc : unescapeString rest
   loop n1 acc (c:cs)
      | c `elem` numericDigits = loop (n1-1) (c:acc) cs
      | otherwise = numericToChar acc : unescapeString (c:cs)
   numericToChar :: String -> Char
   numericToChar = toEnum . readNumeric . reverse

octalDigits, hexDigits :: String
-- | The set of valid octal digits in Python.
octalDigits = "01234567"
-- | The set of valid hex digits in Python.
hexDigits = "0123456789abcdef"
