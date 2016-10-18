module CommandExec where

import qualified Data.ByteString.Lazy as L
import qualified System.Process.ByteString.Lazy as LP

import System.Process
import System.Posix
import System.Posix.Env
import System.Exit
import System.IO
import Exceptions

type Cmd = (FilePath,[String])

has_failed :: ExitCode -> Bool
has_failed (ExitFailure n) =
    (n < 0 || (n > 128 && n < 143))
has_failed ExitSuccess = False

exec :: Cmd -> IO ExitCode
exec (prog, args) = rawSystem prog args

execFromStdinToBuffer :: Cmd                      -- ^ The command line
                         -> L.ByteString             -- ^ Data to pass into the command's std_in
                         -> IO (Maybe L.ByteString)

execFromStdinToBuffer (cmd,args) input =  
  do
    x <- force_catch input
    case x of
      Nothing -> return Nothing 
      Just y ->  do
                    if (L.null y) then return Nothing
                    else (
                       do
                         (_, output, _) <- LP.readProcessWithExitCode cmd args y
                         return $ Just output
                      )


f cmd args y = 
  do
   (ret, _, _) <- LP.readProcessWithExitCode cmd args y
   return $ seq ret ret
 

execfromStdin :: Cmd                      -- ^ The command line
              -> L.ByteString             -- ^ Data to pass into the command's std_in
              -> IO ExitCode

execfromStdin (cmd,args) input =  
  do
    x <- force_catch input
    case x of
      Nothing -> return ExitSuccess
      Just y -> ( do
                    if (L.null y) then return ExitSuccess
                    else ( do
                            z <- mcatch (f cmd args y)
                            case z of
                              Nothing -> return $ ExitFailure 128
                              Just r -> r 
                         )
                 )
