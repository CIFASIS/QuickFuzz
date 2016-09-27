{-# LANGUAGE BangPatterns #-}

module Exceptions where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC8
import Control.Exception 
import Control.DeepSeq
import Control.Seq
import Control.Monad

enc_handler :: SomeException -> IO LC8.ByteString
enc_handler x = return $ LC8.pack ""

dec_handler :: SomeException -> IO (Maybe a)
dec_handler x = return Nothing

handler :: SomeException -> IO ()
handler x = return ()

toNF :: (NFData a) => a -> IO a
toNF = evaluate  . withStrategy rdeepseq

mcatch x = Control.Exception.catch (return $ Just x) dec_handler
force_catch x = Control.Exception.catch ( toNF (Just x)) dec_handler

write :: L.ByteString -> FilePath -> IO ()
write x filename =  Control.Exception.catch (L.writeFile filename x) handler



