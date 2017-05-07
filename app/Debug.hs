{-# LANGUAGE CPP #-}

module Debug (debug) where

import System.IO

debug :: String -> IO ()
debug message =
#ifdef DEBUG
  hPutStrLn stderr message
#else
  return ()
#endif

