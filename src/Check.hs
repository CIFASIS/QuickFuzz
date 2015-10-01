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
handler x = return ()


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

outdir = "outdir"

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
           if size > 0 
              then ( 
                  do 
                    run $ copyFile "vreport.out" (outdir ++ "/" ++ "vreport.out."++ show seed)
                    run $ copyFile filename (outdir ++ "/" ++ filename ++ "."++ show seed)
                    Test.QuickCheck.Monadic.assert True
                  )
              else Test.QuickCheck.Monadic.assert True
           )

fuzzprop filename prog args encode x = 
         noShrinking $ monadicIO $ do
         run $ (L.writeFile filename (encode x))
         size <- run $ getFileSize filename
         if size == 0 
            then Test.QuickCheck.Monadic.assert True 
         else (
           do 
           seed <- run (randomIO :: IO Int)
           ret <- run $ rawSystem "/usr/bin/zzuf" (["-M", "-1", "-r","0.004:0.000001", "-s", (show (seed `mod` 10024))++":"++(show (seed `mod` 10024 + 5)), "-I", filename, "-S", "-T", "5", "-j", "5", prog] ++ args)
           case ret of
              ExitFailure x -> (
                                do
                                 run $ rawSystem "/usr/bin/zzuf" (["-M", "-1", "-r","0.004:0.000001", "-s", (show (seed `mod` 10024))++":"++(show (seed `mod` 10024 + 5)), "-I", filename, "-S", "-T", "5", "-j", "1", prog] ++ args) 
                                 run $ copyFile (filename) (outdir ++ "/" ++ filename ++ "."++ show seed)
                                 Test.QuickCheck.Monadic.assert True
                )
              _             -> Test.QuickCheck.Monadic.assert True
           )

fuzzgdbprop filename prog args encode x = 
         noShrinking $ monadicIO $ do
         run $ (L.writeFile filename (encode x))
         size <- run $ getFileSize filename
         if size == 0 
            then Test.QuickCheck.Monadic.assert True 
         else (
           do 
           seed <- run (randomIO :: IO Int)
           run $ system $ "/usr/bin/zzuf -r 0.004:0.000001 -s" ++ (show (seed `mod` 10024))++":"++(show (seed `mod` 10024 + 1)) ++ "<" ++ filename ++ " > " ++ filename ++ ".fuzzed"
           ret <- run $ rawSystem "/usr/bin/gdb" (["-return-child-result", "-batch-silent", "--tty", "/dev/null", "-ex", "handle SIGTERM nostop", "-ex", "handle SIGCONT nostop", "-ex", "break abort", "-ex", "run", "--args", prog] ++ args)
           case ret of
              ExitFailure x -> (
                                
                                if (x /= 3 && x /= 124) then
                                 do 
                                   ret <- run $ rawSystem "/usr/bin/gdb" (["-batch", "-ex", "break abort", "-ex", "run", "-ex", "bt", "--args", prog] ++ args)

                                   run $ copyFile (filename ++ ".fuzzed") (outdir ++ "/" ++ filename ++ "."++ show seed)
                                   Test.QuickCheck.Monadic.assert True
                                 else
                                   Test.QuickCheck.Monadic.assert True
                )
              _             -> Test.QuickCheck.Monadic.assert True
           )
