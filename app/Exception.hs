module Exception where

import Prelude hiding (writeFile)

import Control.Exception 
import Data.ByteString.Lazy
import Control.DeepSeq
import Control.Seq

import Debug

--import qualified Data.ByteString.Lazy.Char8 as LC8
--import Control.Monad

--enc_handler :: SomeException -> IO LC8.ByteString
--enc_handler x = return $ LC8.pack ""

--dec_handler :: SomeException -> IO (Maybe a)
--dec_handler x = return Nothing
--mcatch x = Control.Exception.catch (return $ Just x) dec_handler

handlePrint :: SomeException -> IO ()
handlePrint = debug . show

handleDecode :: SomeException -> IO (Maybe a)
handleDecode x = handlePrint x >> return Nothing

toNF :: (NFData a) => a -> IO a
toNF = evaluate . withStrategy rdeepseq

forceEvaluation :: ByteString -> IO (Maybe ByteString)
forceEvaluation x = handle handleDecode (toNF (Just x))

--write :: FilePath -> ByteString -> IO ()
--write filename x = handle handlePrint (writeFile filename x)
