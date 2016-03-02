module Exceptions where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC8
import Control.Exception 

enc_handler :: SomeException -> IO LC8.ByteString
enc_handler x = return $ LC8.pack ""

dec_handler :: SomeException -> IO (Maybe a)
dec_handler x = return Nothing

handler :: SomeException -> IO ()
handler x = return ()


