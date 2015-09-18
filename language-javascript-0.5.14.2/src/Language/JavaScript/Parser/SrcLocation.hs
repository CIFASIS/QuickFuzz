{-# LANGUAGE DeriveDataTypeable #-}
module Language.JavaScript.Parser.SrcLocation (
  TokenPosn(..)
  , tokenPosnEmpty
  ) where

import Data.Data

-- | `TokenPosn' records the location of a token in the input text.  It has three
-- fields: the address (number of characters preceding the token), line number
-- and column of a token within the file.
-- Note: The lexer assumes the usual eight character tab stops.

data TokenPosn = TokenPn !Int -- address (number of characters preceding the token)
                         !Int -- line number
                         !Int -- column
        deriving (Eq,Show, Read, Data, Typeable)

tokenPosnEmpty :: TokenPosn
tokenPosnEmpty = TokenPn 0 0 0

