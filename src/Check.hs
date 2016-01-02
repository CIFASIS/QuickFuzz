module Check where

import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import qualified Parallel as P

import Control.Exception
import Control.Monad

import qualified Data.ByteString.Lazy as L
import Data.Maybe

import System.Random
import System.Process
import System.Posix
import System.Exit
import System.Directory

import Data.ByteString.Char8 as L8
import Network hiding (accept, sClose)
import Network.Socket 
import Network.Socket.ByteString (sendAll)
import Control.Concurrent
import Control.Concurrent.Thread.Delay

processPar = P.processPar
parallelism = P.parallelism

getFileSize :: String -> IO FileOffset
getFileSize path = do
    stat <- getFileStatus path
    return (fileSize stat)

handler :: SomeException -> IO ()
handler x = return () --Prelude.putStrLn (show x)--return ()

genprop filename prog args encode outdir x = 
         monadicIO $ do
         seed <- run (randomIO :: IO Int)
         sfilename <- run $ return (outdir ++ "/" ++ filename ++ "." ++ show seed)
         run $ Control.Exception.catch (L.writeFile sfilename (encode x)) handler
         size <- run $ getFileSize sfilename
         if size == 0 
            then (do 
                    run $ removeFile sfilename
                    Test.QuickCheck.Monadic.assert True)
            else
              Test.QuickCheck.Monadic.assert True


checkprop filename prog args encode outdir x = 
         monadicIO $ do
         run $ Control.Exception.catch (L.writeFile filename (encode x)) handler
         size <- run $ getFileSize filename
         if size == 0 
            then Test.QuickCheck.Monadic.assert True 
         else (
           do 
               let varepname = filename ++ ".vreport.out"
               seed <- run (randomIO :: IO Int)
               ret <- run $ rawSystem "/usr/bin/valgrind" (["--log-file="++ varepname, "--quiet", prog] ++ args)
               size <- run $ getFileSize varepname --"vreport.out"
               if size > 0 
                  then ( 
                      do 
                        run $ copyFile varepname (outdir ++ "/" ++ "vreport.out."++ show seed)
                        -- run $ copyFile "vreport.out" (outdir ++ "/" ++ "vreport.out."++ show seed)
                        run $ copyFile filename (outdir ++ "/" ++ filename ++ "."++ show seed)
                        Test.QuickCheck.Monadic.assert True
                      )
                  else Test.QuickCheck.Monadic.assert True
               )

call_honggfuzz filename exprog args seed outdir = 
   rawSystem "honggfuzz" (["-v", "-n", "5", "-N", "50", "-r", "0.00001", "-t","60", "-f", filename,  "-W", outdir, "--", exprog] ++ args)

honggprop :: FilePath -> FilePath -> [String] -> (t -> L.ByteString) -> FilePath -> t -> Property
honggprop filename prog args encode outdir x = 
            noShrinking $ monadicIO $ do
               run $ Control.Exception.catch (L.writeFile filename (encode x)) handler
               size <- run $ getFileSize filename
               when (size > 0) $ do
               --   (Test.QuickCheck.Monadic.assert True) $ do
                 ret <- run $ call_honggfuzz filename prog args undefined outdir
                 Test.QuickCheck.Monadic.assert True


-- write_and_check filename encode x =    
call_zzuf filename exprog args seed outdir = 
  rawSystem "zzuf" (["-M", "-1", "-q", "-r","0.004:0.000001", "-s", show seed ++":"++ show (seed+50), "-I", filename, "-S", "-T", "5", "-j", "1", exprog] ++ args)

zzufprop :: FilePath -> FilePath -> [String] -> (t -> L.ByteString) -> FilePath -> t -> Property
zzufprop filename prog args encode outdir x = 
            noShrinking $ monadicIO $ do
            --run $ createDirectoryIfMissing False outdir
            run $ Control.Exception.catch (L.writeFile filename (encode x)) handler
            size <- run $ getFileSize filename
            unless (size > 0) 
              (Test.QuickCheck.Monadic.assert True)
            seed <- run (randomIO :: IO Int)
            ret <- run $ call_zzuf filename prog args seed outdir
            case ret of
              ExitFailure x ->do
                             run $ copyFile filename (outdir ++ "/" ++ filename ++ "."++ show seed)
                             Test.QuickCheck.Monadic.assert True
              _             -> Test.QuickCheck.Monadic.assert True


radamprop filename prog args encode outdir x = 
         noShrinking $ monadicIO $ do
         run $  Control.Exception.catch (L.writeFile filename (encode x)) handler
         size <- run $ getFileSize filename
         if size == 0 
            then Test.QuickCheck.Monadic.assert True 
         else (
           do 
           seed <- run (randomIO :: IO Int)
           run $ system $ "radamsa" ++ "<" ++ filename ++ " > " ++ filename ++ ".fuzzed"
           ret <- run $ rawSystem prog args
           --run $ putStrLn (show ret)
           case ret of
              ExitFailure x -> (
                                
                                if (x < 0 || x > 128) then
                                 do 
                                   run $ copyFile (filename ++ ".fuzzed") (outdir ++ "/" ++ filename ++ "."++ show seed)
                                   Test.QuickCheck.Monadic.assert True
                                 else
                                   Test.QuickCheck.Monadic.assert True
                )
              _             -> Test.QuickCheck.Monadic.assert True
           )


execprop filename prog args encode outdir x = 
         noShrinking $ monadicIO $ do
         --run $ Prelude.putStrLn (show x)
         run $  Control.Exception.catch (L.writeFile filename (encode x)) handler
         size <- run $ getFileSize filename
         if size == 0 
            then Test.QuickCheck.Monadic.assert True 
         else (
           do 
           seed <- run (randomIO :: IO Int)
           --run $ system $ "/usr/bin/zzuf -r 0.004:0.000001 -s" ++ (show (seed `mod` 10024))++":"++(show (seed `mod` 10024 + 1)) ++ "<" ++ filename ++ " > " ++ filename ++ ".fuzzed"
           ret <- run $ rawSystem prog args
           --run $ putStrLn (show ret)
           case ret of
              ExitFailure x -> (
                                
                                if ((x < 0 || x > 128) && x /= 143) then
                                 do 
                                   run $ copyFile filename (outdir ++ "/" ++ filename ++ "."++ show seed)
                                   Test.QuickCheck.Monadic.assert True
                                 else
                                   Test.QuickCheck.Monadic.assert True
                )
              _             -> Test.QuickCheck.Monadic.assert True
           )


serve :: PortNumber -> [L8.ByteString] -> IO ()
serve port xs = withSocketsDo $ do
    sock <- listenOn $ PortNumber port
    loop sock xs

loop sock (x:xs) = do
   Prelude.putStrLn "Accepting connection.."
   (conn, _) <- accept sock
   forkIO $ body conn
   loop sock xs
  where
   body c = do sendAll c x
               sClose c

loop _ [] = error "Empty list!"

serveprop filename port _ encode x =  
        noShrinking $ monadicIO $ do
           run $ serve port (encode x)
           Test.QuickCheck.Monadic.assert True
