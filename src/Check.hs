module Check where

import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Control.Exception

import qualified Data.ByteString.Lazy as L

import System.Random
import System.Process
import System.Posix
import System.Exit
import System.Directory

getFileSize :: String -> IO FileOffset
getFileSize path = do
    stat <- getFileStatus path
    return (fileSize stat)

handler :: SomeException -> IO ()
handler _ = return ()


{-
zzuf prog args filename = do
                            seed <- run (randomIO :: IO Int)
                            ret <- system "/usr/bin/zzuf -r 0.001:0.00000001 -s" ++ (show (seed `mod` 10024))++":"++(show (seed `mod` 10024 + 1)) ++ "< " ++ filename ++ " > " ++ filename + ".fuzzed" 
-}

genprop filename prog args encode dir x = 
         monadicIO $ do
         seed <- run (randomIO :: IO Int)
         sfilename <- run $ return (dir ++ "/" ++ filename ++ "." ++ (show seed))
         run $ Control.Exception.catch (L.writeFile sfilename (encode x)) handler
         size <- run $ getFileSize sfilename
         if size == 0 
            then (do 
                    run $ removeFile sfilename
                    Test.QuickCheck.Monadic.assert True)
            else
              Test.QuickCheck.Monadic.assert True


checkprop filename prog args encode x = 
         monadicIO $ do
         run $ Control.Exception.catch (L.writeFile filename (encode x)) handler
         size <- run $ getFileSize filename
         if size == 0 
            then Test.QuickCheck.Monadic.assert True 
         else (
           do 
           seed <- run (randomIO :: IO Int)
           ret <- run $ rawSystem "/usr/bin/valgrind" (["--log-file=vreport.out", "--quiet", prog] ++ args)
           size <- run $ getFileSize "vreport.out"
           if size == 0 
              then Test.QuickCheck.Monadic.assert True
              else Test.QuickCheck.Monadic.assert False
           )

fuzzprop filename prog args encode x = 
         noShrinking $ monadicIO $ do
         run $ Control.Exception.catch (L.writeFile filename (encode x)) handler
         size <- run $ getFileSize filename
         if size == 0 
            then Test.QuickCheck.Monadic.assert True 
         else (
           do 
           seed <- run (randomIO :: IO Int)
           --ret <- run $ rawSystem "/usr/bin/file" [filename]
           --run $ putStrLn (show ret)
           ret <- run $ rawSystem "/usr/bin/zzuf" (["-q", "-M", "4000", "-r","0.004:0.000001", "-s", (show (seed `mod` 10024))++":"++(show (seed `mod` 10024 + 100)), "-c", "-S", "-T", "15", "-j", "5", prog] ++ args)
           case ret of
              ExitFailure y -> Test.QuickCheck.Monadic.assert True
              _             -> Test.QuickCheck.Monadic.assert True
           )
