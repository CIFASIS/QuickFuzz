module Check where

import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Control.Exception

import qualified Data.ByteString.Lazy as L

import System.Random
import System.Process
import System.Posix
import System.Exit

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


absprop filename prog args encode x = 
         monadicIO $ do
         run $ Control.Exception.catch (L.writeFile filename (encode x)) handler
         size <- run $ getFileSize filename
         if size == 0 
            then Test.QuickCheck.Monadic.assert True 
         else (
           do 
           seed <- run (randomIO :: IO Int)
           --ret <- run $ rawSystem "/usr/bin/zzuf" (["-q", "-M", "-1", "-r","0.001:0.00000001", "-s", (show (seed `mod` 10024))++":"++(show (seed `mod` 10024 + 100)), "-c", "-S", "-T", "15", "-j", "5", prog] ++ args)
           --ret <- run $ rawSystem "/usr/bin/zzuf" (["-q", "-r","0.001:0.00000001", "-s", (show (seed `mod` 10024))++":"++(show (seed `mod` 10024 + 100)), "-c", "-S", "-T", "10", "-j", "5", prog] ++ args)

           ret <- run $ rawSystem "tcreator2" [ "--copy", ("jasper-"++show seed), "/usr/bin/jasper --input file:/home/vagrant/QuickFuzz/"++ filename ++" --output-format pnm", "jpeg"]
           --ret <- run $ rawSystem "fextractor" (["--show-stdout", "--dynamic", "--timeout", "60", "--out-file","/home/vagrant/QuickFuzz/jpeg-jpegtopnm.csv", "jpeg/_usr_bin_jpegtopnm:0"])


           --ret <- run $ rawSystem "fextractor" (["--show-stdout", "--dynamic", "--timeout", "60", "--out-file","/home/vagrant/QuickFuzz/jpeg-jpegtopnm.csv", "jpeg/_usr_bin_jpegtopnm:0"])
           --ret <- run $ rawSystem "/usr/bin/valgrind" (["--quiet", prog] ++ args)         
           case ret of
              ExitFailure y -> Test.QuickCheck.Monadic.assert True
              _             -> Test.QuickCheck.Monadic.assert True
           )
