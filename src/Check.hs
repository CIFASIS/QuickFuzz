module Check where

import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Control.Exception

import qualified Data.ByteString.Lazy as L

import System.Random
import System.Process
import System.Exit


handler :: SomeException -> IO ()
handler _ = return ()

absprop filename prog args encode x = 
         monadicIO $ do
         run $ Control.Exception.catch (L.writeFile filename (encode x)) handler
         --run $ (L.writeFile filename (encode x))

         seed <- run (randomIO :: IO Int)
         ret <- run $ rawSystem "/usr/bin/zzuf" (["-M", "-1", "-r","0.001:0.00000001", "-s", (show (seed `mod` 10024))++":"++(show (seed `mod` 10024 + 100)), "-c", "-S", "-T", "15", "-j", "5", prog] ++ args)
         --ret <- run $ rawSystem "/usr/bin/zzuf" (["-q", "-r","0.001:0.00000001", "-s", (show (seed `mod` 10024))++":"++(show (seed `mod` 10024 + 100)), "-c", "-S", "-T", "10", "-j", "5", prog] ++ args)

         --ret <- run $ rawSystem "fextractor" (["--show-stdout", "--dynamic", "--timeout", "60", "--out-file","/home/vagrant/QuickFuzz/jpeg-openjpeg-utils.csv", "jpeg/_usr_bin_j2k_to_image:0/"])
         --ret <- run $ rawSystem "/usr/bin/valgrind" (["--quiet", prog] ++ args)         
         case ret of
            ExitFailure y -> Test.QuickCheck.Monadic.assert False
            _             -> Test.QuickCheck.Monadic.assert True
