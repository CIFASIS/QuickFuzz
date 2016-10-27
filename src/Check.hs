{-# LANGUAGE CPP                #-}

module Check where

import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run, PropertyM (..) )

import qualified Parallel as P

import Control.Monad

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC8

import Data.Maybe

import System.Random
import System.Process
import System.Posix
import System.Posix.Env
import System.Directory
import System.IO.Unsafe


import Data.Char (chr)
import Data.ByteString.Char8 as L8

#ifdef NET

import Network hiding (accept, sClose)
import Network.Socket hiding (send, sendTo, recv, recvFrom) 
import Network.Socket.ByteString (send, sendTo, recv, recvFrom, sendAll)
import Control.Concurrent
import Control.Concurrent.Thread.Delay

#endif

import CommandExec
import Exceptions
import Mutation

processPar = P.processPar
parallelism = P.parallelism

getFileSize :: String -> IO FileOffset
getFileSize path = do
    stat <- getFileStatus path
    return (fileSize stat)

-- Special hooks

save filename outdir = 
  do
     seed <- randomIO :: IO Int
     copyFile filename (outdir ++ "/" ++ show seed ++ "." ++ filename)
 
report value filename outdir =
  do
     seed <- randomIO :: IO Int
     LC8.writeFile (outdir ++ "/" ++ show seed ++ "." ++ filename ++ ".val") (LC8.pack (show value))
     copyFile filename (outdir ++ "/" ++ show seed ++ "." ++ filename)
     copyFile filename (outdir ++ "/last")

vreport value report filename outdir =
  do
     seed <- randomIO :: IO Int
     LC8.writeFile (outdir ++ "/" ++ show seed ++ "." ++ filename ++ ".val") (LC8.pack (show value))
     copyFile filename (outdir ++ "/" ++ show seed ++ "." ++ filename)
     copyFile report (outdir ++ "/" ++ show seed ++ "." ++ report)

    
freport value orifilename filename outdir =
  do
     seed <- randomIO :: IO Int
     LC8.writeFile (outdir ++ "/" ++ show seed ++ "." ++ filename ++ ".val") (LC8.pack (show value))
     copyFile orifilename (outdir ++ "/" ++ show seed ++ "." ++ filename ++ ".ori")
     copyFile filename (outdir ++ "/" ++ show seed ++ "." ++ filename)
     copyFile filename (outdir ++ "/last")

rreport filename outdir =
  do
     seed <- randomIO :: IO Int
     copyFile filename (outdir ++ "/red." ++ show seed ++ "." ++ filename)
 

-- Properties

prop_MutateGen :: (Mutation a, Show a, Arbitrary a) => FilePath -> Cmd -> (a -> L.ByteString) -> FilePath -> [a] -> a -> Property
prop_MutateGen filename pcmd encode outdir vals x = 
         noShrinking $ monadicIO $ do
         r <- run (randomIO :: IO Int)
         idx <- run $ return (r `mod` Prelude.length vals)
         --run $ print "Mutating.."

         x <- run $ return $ vals !! idx
         --run $ print "Reading.."

         y <- run $ generate $ resize 100 $ mutt x

         --run $ print ((show x) == (show y))
         run $ write (encode y) filename
         size <- run $ getFileSize filename


         if size == 0 --((show x) == (show y))
 
            then assert True
         else (
           do
            ret <- run $ exec pcmd
            if not (has_failed ret) then assert True else
              (do run $ report x filename outdir
                  assert False)

             {-run $ write (encode y) filename
             run $ save filename outdir
             assert True-}
             )
 


execHonggfuzz filename (prog,args) seed outdir = 
   rawSystem "honggfuzz" (["-q", "-v", "-n", "2", "-N", "5", "-r", "0.00001", "-t","60", "-f", filename,  "-W", outdir, "--", prog] ++ args)

propHonggfuzzExec :: Show a => FilePath -> Cmd -> (a -> L.ByteString) -> FilePath -> a -> Property
propHonggfuzzExec filename pcmd encode outdir x = 
         noShrinking $ monadicIO $ do
         run $ write (encode x) filename
         size <- run $ getFileSize filename
         if size == 0 
            then assert True
         else (
           do 
             ret <- run $ execHonggfuzz filename pcmd undefined outdir
             assert True
             )
         

execZzuf seed = execFromStdinToBuffer ("zzuf", ["-r", "0.004:0.000001", "-s", show seed])

propZzufExec :: Show a => FilePath -> Cmd -> (a -> L.ByteString) -> FilePath -> a -> Property
propZzufExec [] pcmd encode outdir x = 
         monadicIO $ do
         seed <- run (randomIO :: IO Int)
         y <- run $ execZzuf seed (encode x) 
         if isNothing y
            then assert True
         else (
           do 
           ret <- run $ execfromStdin pcmd (fromJust y)
           if not (has_failed ret) then assert True else
             (do run $ write (fromJust y) ("stdin." ++ show seed)
                 run $ report x ("stdin." ++ show seed) outdir
                 assert False)
           )
propZzufExec filename pcmd encode outdir x = 
         monadicIO $ do
         seed <- run (randomIO :: IO Int)
         x <- run $ execZzuf seed (encode x) 
         if isNothing x
            then assert True
         else (
           do 
           run $ write (fromJust x) filename
           ret <- run $ exec pcmd
           if not (has_failed ret) then assert True else
             (do run $ report x filename outdir
                 assert False)
           )




execLTrace (prog,args) outfile =
   do
     rawSystem "/usr/bin/ltrace" (["-o"++ outfile, "-e","execve",prog] ++ args)
     rawSystem "/bin/cat" [outfile]

propLTraceExec :: Show a => FilePath -> Cmd -> (a -> L.ByteString) -> FilePath -> a -> Property
propLTraceExec filename pcmd encode outdir x = 
         noShrinking $ monadicIO $ do
         let rep_filename = filename ++ ".lreport.out"
         run $ write (encode x) filename
         run $ execLTrace pcmd rep_filename
         size <- run $ getFileSize rep_filename
         assert True


execValgrind (prog,args) outfile =
 rawSystem "/usr/bin/valgrind" (["--log-file="++ outfile, "--quiet", prog] ++ args)

propValgrindExec :: Show a => FilePath -> Cmd -> (a -> L.ByteString) -> FilePath -> a -> Property
propValgrindExec filename pcmd encode outdir x =
         noShrinking $ monadicIO $ do
         let rep_filename = filename ++ ".vreport.out"
         run $ write (encode x) filename
         run $ execValgrind pcmd rep_filename
         size <- run $ getFileSize rep_filename
         case size of
             0 -> assert True
             _ -> do run $ vreport x rep_filename filename outdir
                     assert False
 

execRadamsa = execFromStdinToBuffer ("radamsa", [])
--rawSystem "radamsa" [infile, "-o", outfile]

propRadamsaExec :: Show a => FilePath -> Cmd -> (a -> L.ByteString) -> FilePath -> a -> Property
propRadamsaExec [] pcmd encode outdir x = 
         noShrinking $ monadicIO $ do
         seed <- run (randomIO :: IO Int)
         y <- run $ execRadamsa (encode x)
         if isNothing y
            then assert True
         else (
           do 
           ret <- run $ execfromStdin pcmd (fromJust y)
           if not (has_failed ret) then assert True else
             (do run $ write (fromJust y) ("stdin." ++ show seed)
                 run $ report x ("stdin." ++ show seed) outdir
                 assert True)
           )
propRadamsaExec filename pcmd encode outdir x = 
         noShrinking $ monadicIO $ do
         y <- run $ execRadamsa (encode x)
         if isNothing y
            then assert True
         else (
           do 
           run $ write (fromJust y) filename
           ret <- run $ exec pcmd
           case not (has_failed ret) of
              False -> (do 
                        run $ report x filename outdir
                        assert False
               )
              _             -> assert True
           )

propExec :: Show a => FilePath -> Cmd -> (a -> L.ByteString) -> FilePath -> a -> Property
propExec [] pcmd encode outdir x = 
         monadicIO $ do
         seed <- run $ (randomIO :: IO Int)
         ret <- run $ execfromStdin pcmd (encode x)
         case not (has_failed ret) of
            False -> do
                          run $ write (encode x) ("stdin." ++ show seed)
                          run $ report x ("stdin." ++ show seed) outdir
                          assert True
            _             -> assert True

propExec filename pcmd encode outdir x = 
         monadicIO $ do
         run $ write (encode x) filename
         size <- run $ getFileSize filename
         if size == 0 
            then assert True
         else (
           do 
           ret <- run $ exec pcmd
           case not (has_failed ret) of
              False -> (do 
                        run $ report x filename outdir
                        assert False
               )
              _             -> assert True
           )


propEnvExec :: Show a => FilePath -> Cmd -> (a -> L.ByteString) -> FilePath -> a -> Property
propEnvExec filename pcmd encode outdir x = 
         monadicIO $ do
         str <- return $ Prelude.take 2000 $ Prelude.map (chr . fromEnum) . L.unpack $ (encode x) 
         run $ clearEnv
         run $ setEnv "ASAN_OPTIONS" "'abort_on_error=1'" True
         run $ setEnv "X"  ("() { "++str++";}") True
         ret <- run $ exec pcmd 
         case not (has_failed ret) of
              False -> (do 
                        run $ report x filename outdir
                        assert False
               )
              _             -> assert True 

propRed :: Show a => FilePath -> Cmd -> (a -> L.ByteString) -> FilePath -> a -> Property
propRed filename pcmd encode outdir x = 
         monadicIO $ do
         run $ write (encode x) filename
         ret <- run $ exec pcmd
         case not (has_failed ret) of
              False -> (do 
                        run $ rreport filename outdir
                        assert False
               )
              _             -> assert True
         
propRed [] pcmd encode outdir x = 
         monadicIO $ do
         seed <- run $ (randomIO :: IO Int)
         ret <- run $ execfromStdin pcmd (encode x)
         case not (has_failed ret) of
              False -> (do
                          run $ write (encode x) ("stdin." ++ show seed)
                          run $ rreport ("stdin." ++ show seed) outdir
                          assert False
               )
              _             -> assert True
         




propGen :: Show a => FilePath -> Cmd -> (a -> L.ByteString) -> FilePath -> a -> Property
propGen filename pcmd encode outdir x = 
         noShrinking $ monadicIO $ do
         run $ write (encode x) filename
         size <- run $ getFileSize filename
         if size == 0 
            then assert True
         else (
           do
           run $ save filename outdir
           assert True
           )

#ifdef NET

serve :: PortNumber -> [L8.ByteString] -> IO ()
serve port xs = withSocketsDo $ do
    sock <- listenOn $ PortNumber port
    serveLoop sock xs

serveLoop sock (x:xs) = do
   Prelude.putStrLn "Accepting connection.."
   (conn, _) <- accept sock
   forkIO $ body conn
   serveLoop sock xs
  where
   body c = do sendAll c x
               sClose c

serveLoop _ [] = error "Empty list!"

serveProp port _ encode x =  
        noShrinking $ monadicIO $ do
           run $ serve port (encode x)
           Test.QuickCheck.Monadic.assert True

cconnect :: PortNumber -> String -> [L8.ByteString] -> IO ()
cconnect port host xs = withSocketsDo $ do
    Prelude.putStrLn host
    Prelude.putStrLn (show port)
    addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)

    let serverAddr = Prelude.head addrInfo
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    --sock <- conn $ PortNumber port
    connect sock (addrAddress serverAddr)
    cconectLoop sock xs

cconectLoop sock (x:xs) = do
   Prelude.putStrLn "Sending data .."
   send sock x
   --(conn, _) <- accept sock
   --forkIO $ body conn
   cconnectLoop sock xs
  --where
  -- body c = do sendAll c x
  --             sClose c

cconnectLoop _ [] = error "Empty list!"

cconnectprop port host encode x =  
        noShrinking $ monadicIO $ do
           run $ cconnect port host (encode x)
           Test.QuickCheck.Monadic.assert True

#endif
