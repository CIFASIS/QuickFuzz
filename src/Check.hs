module Check where

import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)

--import Control.Monad.Zip
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
         r <- run (randomIO :: IO Int)
         ret <- run $ rawSystem "/usr/bin/zzuf" (["-s", (show (r `mod` 10024))++":"++(show (r `mod` 10024 + 50)), "-M", "-1", "-c", "-S", "-T", "60", "-j", "5", prog] ++ args)
         case ret of
            ExitFailure y -> Test.QuickCheck.Monadic.assert False 
            _             -> Test.QuickCheck.Monadic.assert True
