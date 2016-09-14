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
import qualified System.Process.ByteString.Lazy as LP
import System.Posix
import System.Posix.Env
import System.Exit
import System.Directory
import System.IO
import System.IO.Unsafe
import System.IO.Error
import System.Timeout

import Data.Char (chr)
import Data.ByteString.Char8 as L8

import Foreign.Marshal.Alloc
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

--bhandler :: SomeException -> IO L.ByteString
--bhandler x = return (LC8.pack "") --Prelude.putStrLn (show x)--return ()


--quickhandler x = Nothing

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

freport value oripayload filename outdir =
  do
    seed <- (randomIO :: IO Int)
    LC8.writeFile (outdir ++ "/" ++ show seed ++ "." ++ filename ++ ".val") (LC8.pack (show value))
    LC8.writeFile (outdir ++ "/" ++ show seed ++ "." ++ filename ++ ".ori") oripayload
    copyFile filename (outdir ++ "/" ++ show seed ++ "." ++ filename)
    copyFile filename (outdir ++ "/last")

rreport value filename outdir =
  do
    seed <- (randomIO :: IO Int)
    copyFile filename (outdir ++ "/red." ++ show seed ++ "." ++ filename)

-- | Executes an external command and writes the command's standard out to a file at outpath
execToFile :: String         -- ^ The path to the command
           -> [String]       -- ^ Arguments for the command
           -> LC8.ByteString -- ^ Data to pass into the command's std_in
           -> String         -- ^ Path of file to write command's std_out to
           -> IO ExitCode    -- ^ Exit code of the command
execToFile cmdpath args payload outfile =
  do
    (exitCode, stdout, _) <- LP.readProcessWithExitCode cmdpath args payload
    L.writeFile outfile stdout
    return exitCode


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

execZzuf :: LC8.ByteString -> String -> IO ExitCode
execZzuf payload outfile =
  do
    seed <- randomIO :: IO Int
    execToFile "zzuf" ["--stdin", "--ratio", "0.004:0.000001", "--seed", show seed] payload outfile

propZzufExec :: Show a => FilePath -> Cmd -> (a -> L.ByteString) -> FilePath -> a -> Property
propZzufExec filename pcmd encode outdir x =
  noShrinking $ monadicIO $ do
    let encoded = encode x
    run $ execZzuf encoded filename
    ret <- run $ exec pcmd
    case not (has_failed ret) of
      False -> (do 
                  run $ freport x encoded filename outdir
                  assert False
               )
      _     -> assert True

execLTrace :: Cmd -> String -> IO ExitCode
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
         case size of
             _     -> (assert True)

execValgrind :: Cmd -> String -> IO ExitCode
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
             0     -> (assert True)
             _     -> (do
                        run $ vreport x rep_filename filename outdir
                        assert False)

execRadamsa :: LC8.ByteString -> String -> IO ExitCode
execRadamsa payload outfile = do
  seed <- randomIO :: IO Int
  execToFile "radamsa" ["-o", outfile] payload outfile

propRadamsaExec :: Show a => FilePath -> Cmd -> (a -> L.ByteString) -> FilePath -> a -> Property
propRadamsaExec filename pcmd encode outdir x =
  noShrinking $ monadicIO $ do
    let encoded = encode x
    run $ execRadamsa encoded filename
    ret <- run $ exec pcmd
    case not (has_failed ret) of
      False -> (do 
                  run $ freport x encoded filename outdir
                  assert False)
      _    -> assert True


propExec :: Show a => FilePath -> Cmd -> (a -> L.ByteString) -> FilePath -> a -> Property
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
                        run $ rreport x filename outdir
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
