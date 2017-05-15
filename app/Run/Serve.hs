{-# LANGUAGE ExistentialQuantification #-}
module Run.Serve (runServe) where

import Prelude hiding (writeFile)
import Data.ByteString.Lazy (writeFile, toStrict)

import Control.Monad
import Control.Exception

import Network hiding (accept, sClose)
import Network.Socket hiding (send, sendTo, recv, recvFrom) 
import Network.Socket.ByteString (send, sendTo, recv, recvFrom, sendAll)
import Control.Concurrent (forkIO)
import System.IO (hSetBuffering, stdout,  BufferMode(..))

import Test.QuickCheck (generate, resize, infiniteListOf)
import Test.QuickFuzz.Gen.FormatInfo

import Args
import Debug
import Exception


--serve :: PortNumber -> [ByteString] -> IO ()
serve port xs = withSocketsDo $ do
    sock <- listenOn $ PortNumber port
    serveLoop sock xs

serveLoop sock (x:xs) = do
   putStr "."
   (conn, _) <- accept sock
   forkIO $ body conn
   serveLoop sock xs
  where
   body c = do sendAll c (toStrict x)
               close c

serveLoop _ [] = error "Empty list!"

toPortNumber :: Int -> PortNumber
toPortNumber = fromInteger . toInteger

runServe :: QFCommand -> FormatInfo base actions-> IO ()
runServe info fmt = do 
    hSetBuffering stdout NoBuffering
    putStrLn $ "Serving " ++ (format info) ++ "files through port " ++ (show (port info))
    xs <- generate $ infiniteListOf (random fmt)
    serve (toPortNumber (port info)) (map (encode fmt) xs)
    
    --mkName <- nameMaker fmt
    --createDirectoryIfMissing True (outDir cmd)
    --forM_ [1..(genQty cmd)] $ \n -> do 
        --val <- generate $ resize (linear cmd n) (random fmt)
        --write (outDir cmd </> mkName n) (encode fmt val)
        --printStep cmd mkName n
