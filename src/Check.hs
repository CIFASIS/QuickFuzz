{-# LANGUAGE CPP                #-}

module Check where

import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run, PropertyM (..) )

import qualified Parallel as P

import Control.Exception (catch)
import Control.Monad

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.Maybe

import System.Random
import System.Process
import System.Posix
import System.Posix.Env
import System.Exit
import System.Directory
import System.IO.Unsafe
import System.Timeout

import Data.Char (chr)
import Data.ByteString.Char8 as L8

#ifdef NET

import Network hiding (accept, sClose)
import Network.Socket hiding (send, sendTo, recv, recvFrom) 
import Network.Socket.ByteString (send, sendTo, recv, recvFrom, sendAll)
import Control.Concurrent
import Control.Concurrent.Thread.Delay

#endif

import Exceptions
import Mutation

processPar = P.processPar
parallelism = P.parallelism

getFileSize :: String -> IO FileOffset
getFileSize path = do
    stat <- getFileStatus path
    return (fileSize stat)

type Cmd = (FilePath,[String])

has_failed :: ExitCode -> Bool
has_failed (ExitFailure n) =
    (n < 0 || (n > 128 && n < 143))
has_failed ExitSuccess = False

write :: L.ByteString -> FilePath -> IO ()
write x filename =  Control.Exception.catch (L.writeFile filename x) handler

exec :: Cmd -> IO ExitCode
exec (prog, args) = rawSystem prog args

save filename outdir = 
  do
     seed <- (randomIO :: IO Int)
     copyFile filename (outdir ++ "/" ++ show seed ++ "." ++ filename)
 
report value filename outdir =
  do
     seed <- (randomIO :: IO Int)
     LC8.writeFile (outdir ++ "/" ++ show seed ++ "." ++ filename ++ ".val") (LC8.pack (show value))
     copyFile filename (outdir ++ "/" ++ show seed ++ "." ++ filename)

vreport value report filename outdir =
  do
     seed <- (randomIO :: IO Int)
     LC8.writeFile (outdir ++ "/" ++ show seed ++ "." ++ filename ++ ".val") (LC8.pack (show value))
     copyFile filename (outdir ++ "/" ++ show seed ++ "." ++ filename)
     copyFile report (outdir ++ "/" ++ show seed ++ "." ++ report)

    
freport value orifilename filename outdir =
  do
     seed <- (randomIO :: IO Int)
     LC8.writeFile (outdir ++ "/" ++ show seed ++ "." ++ filename ++ ".val") (LC8.pack (show value))
     copyFile orifilename (outdir ++ "/" ++ show seed ++ "." ++ filename ++ ".ori")
     copyFile filename (outdir ++ "/" ++ show seed ++ "." ++ filename)
     copyFile filename (outdir ++ "/last")

rreport value filename outdir =
  do
     seed <- (randomIO :: IO Int)
     copyFile filename (outdir ++ "/red." ++ show seed ++ "." ++ filename)
 

prop_MutateGen :: (Mutation a, Show a, Arbitrary a) => FilePath -> Cmd -> (a -> L.ByteString) -> FilePath -> [a] -> a -> Property

prop_MutateGen filename pcmd encode outdir vals x = 
         noShrinking $ monadicIO $ do
         r <- run (randomIO :: IO Int)
         idx <- run $ return (r `mod` (Prelude.length vals))
         --run $ print "Mutating.."

         x <- run $ return $ vals !! idx
         --run $ print "Reading.."

         y <- run $ generate $ resize 100 $ mutt $ x

         --run $ print ((show x) == (show y))
         run $ write (encode y) filename
         size <- run $ getFileSize filename


         if size == 0 --((show x) == (show y))
 
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
             
             {-run $ write (encode y) filename
             run $ save filename outdir
             assert True-}
             )
 


exec_honggfuzz filename (prog,args) seed outdir = 
   rawSystem "honggfuzz" (["-q", "-v", "-n", "2", "-N", "5", "-r", "0.00001", "-t","60", "-f", filename,  "-W", outdir, "--", prog] ++ args)

prop_HonggfuzzExec :: Show a => FilePath -> Cmd -> (a -> L.ByteString) -> FilePath -> a -> Property
prop_HonggfuzzExec filename pcmd encode outdir x = 
         noShrinking $ monadicIO $ do
         run $ write (encode x) filename
         size <- run $ getFileSize filename
         if size == 0 
            then assert True
         else (
           do 
             ret <- run $ exec_honggfuzz filename pcmd undefined outdir
             assert True
             )
         

exec_zzuf infile outfile = 
  system $ "zzuf  -r 0.004:0.000001 -s" ++ show seed ++ " < " ++ infile ++ " > " ++ outfile
    where seed = unsafePerformIO (randomIO :: IO Int)

prop_ZzufExec :: Show a => FilePath -> Cmd -> (a -> L.ByteString) -> FilePath -> a -> Property
prop_ZzufExec filename pcmd encode outdir x = 
         monadicIO $ do
         let tmp_filename = ".qf." ++ filename

         run $ write (encode x) tmp_filename
         run $ exec_zzuf tmp_filename filename
         ret <- run $ exec pcmd
         case not (has_failed ret) of
           False -> (do 
                        run $ freport x tmp_filename filename outdir
                        assert False
               )
           _     -> assert True




exec_ltrace (prog,args) outfile =
   do
     rawSystem "/usr/bin/ltrace" (["-o"++ outfile, "-e","execve",prog] ++ args)
     rawSystem "/bin/cat" [outfile]

prop_LTraceExec :: Show a => FilePath -> Cmd -> (a -> L.ByteString) -> FilePath -> a -> Property
prop_LTraceExec filename pcmd encode outdir x = 
         noShrinking $ monadicIO $ do
         let rep_filename = filename ++ ".lreport.out"
         run $ write (encode x) filename
         run $ exec_ltrace pcmd rep_filename
         size <- run $ getFileSize rep_filename
         case size of
             _     -> (assert True)


exec_valgrind (prog,args) outfile =
 rawSystem "/usr/bin/valgrind" (["--log-file="++ outfile, "--quiet", prog] ++ args)

prop_ValgrindExec :: Show a => FilePath -> Cmd -> (a -> L.ByteString) -> FilePath -> a -> Property
prop_ValgrindExec filename pcmd encode outdir x =
         noShrinking $ monadicIO $ do
         let rep_filename = filename ++ ".vreport.out"
         run $ write (encode x) filename
         run $ exec_valgrind pcmd rep_filename
         size <- run $ getFileSize rep_filename
         case size of
             0     -> (assert True)
             _     -> (do
                        run $ vreport x rep_filename filename outdir
                        assert False)
 

exec_radamsa infile outfile =
 rawSystem "radamsa" [infile, "-o", outfile]

prop_RadamsaExec :: Show a => FilePath -> Cmd -> (a -> L.ByteString) -> FilePath -> a -> Property
prop_RadamsaExec filename pcmd encode outdir x = 
         noShrinking $ monadicIO $ do
         let tmp_filename = ".qf." ++ filename

         run $ write (encode x) tmp_filename
         run $ exec_radamsa tmp_filename filename
         ret <- run $ exec pcmd
         case not (has_failed ret) of
             False -> (do 
                        run $ freport x tmp_filename filename outdir
                        assert False)
             _     -> (assert True) 
           

prop_Exec :: Show a => FilePath -> Cmd -> (a -> L.ByteString) -> FilePath -> a -> Property
prop_Exec filename pcmd encode outdir x = 
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


prop_EnvExec :: Show a => FilePath -> Cmd -> (a -> L.ByteString) -> FilePath -> a -> Property
prop_EnvExec filename pcmd encode outdir x = 
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

prop_Red :: Show a => FilePath -> Cmd -> (a -> L.ByteString) -> FilePath -> a -> Property
prop_Red filename pcmd encode outdir x = 
         monadicIO $ do
         run $ write (encode x) filename
         ret <- run $ exec pcmd
         case not (has_failed ret) of
              False -> (do 
                        run $ rreport x filename outdir
                        assert False
               )
              _             -> assert True
         



prop_Gen :: Show a => FilePath -> Cmd -> (a -> L.ByteString) -> FilePath -> a -> Property
prop_Gen filename pcmd encode outdir x = 
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
    serve_loop sock xs

serve_loop sock (x:xs) = do
   Prelude.putStrLn "Accepting connection.."
   (conn, _) <- accept sock
   forkIO $ body conn
   serve_loop sock xs
  where
   body c = do sendAll c x
               sClose c

serve_loop _ [] = error "Empty list!"

serveprop port _ encode x =  
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
    cconect_loop sock xs

cconect_loop sock (x:xs) = do
   Prelude.putStrLn "Sending data .."
   send sock x
   --(conn, _) <- accept sock
   --forkIO $ body conn
   cconnect_loop sock xs
  --where
  -- body c = do sendAll c x
  --             sClose c

cconnect_loop _ [] = error "Empty list!"

cconnectprop port host encode x =  
        noShrinking $ monadicIO $ do
           run $ cconnect port host (encode x)
           Test.QuickCheck.Monadic.assert True

#endif
