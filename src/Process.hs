{-# LANGUAGE CPP                #-}

module Process where

import Args
import Mutation
import Test.QuickCheck
import Check
import CommandExec
import Data.List.Split
import Data.Maybe
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import ByteString

import System.Directory hiding (listDirectory, withCurrentDirectory)

#ifdef NET

import Network.Socket

#endif

import Control.Exception --( bracket, bracketOnError, evaluate, catch )
import Exceptions

readCoefFileName :: FilePath -> IO ([Int])
readCoefFileName f  = do 
                       contents <- readFile f
                       return $ (map read) . words $ contents

listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
  (Prelude.filter f) <$> (getDirectoryContents path)
  where f filename = filename /= "." && filename /= ".."


withCurrentDirectory :: FilePath  -- ^ Directory to execute in
                     -> IO a      -- ^ Action to be executed
                     -> IO a
withCurrentDirectory dir action =
  bracket getCurrentDirectory setCurrentDirectory $ \ _ -> do
    setCurrentDirectory dir
    action


decodeFile mdecode filename =
  do
    x <- BS.readFile filename
    --print (BS.length x)
    --print filename
    x <- catch (evaluate $ Just $ mdecode x) dec_handler
    return x

str2prop
  :: Show t => 
       String
    -> FilePath
    -> Cmd
    -> (t -> BSL.ByteString)
    -> FilePath
    -> t
    -> Property

str2prop "zzuf" = prop_ZzufExec
str2prop "radamsa" = prop_RadamsaExec
str2prop "valgrind" = prop_ValgrindExec
str2prop "ltrace" = prop_LTraceExec
str2prop "gen" = prop_Gen
str2prop "env" = prop_EnvExec
str2prop "exec" = prop_Exec
str2prop "honggfuzz" = prop_HonggfuzzExec
str2prop _ = prop_Exec


reduce filename
       (prog,args)
       outdir 
       r@(Failure {}) = do
                         x <- BSL.readFile (outdir ++ "/last")
                         r <- quickCheckResult (forAllShrink (return x) shrink $ prop_Red filename (prog,args) id outdir)
                         print r
                         return r

   
reduce _ _ _ r = return r

process_custom :: Show a
    => Gen a -> ((a -> BSL.ByteString),(BS.ByteString -> a)) 
    -> Bool -> FilePath -> String -> String -> Int
    -> Int -> FilePath -> FilePath -> IO Result 
process_custom gen (mencode,mdecode) par filename cmd prop maxSuccess maxSize outdir seeds =

        let spl = splitOn " " cmd
            (prog, args) = (Prelude.head spl, Prelude.tail spl)
            qcconf = stdArgs { maxSuccess = maxSuccess , maxSize = maxSize, chatty = not par } in
        createDirectoryIfMissing True outdir >>
        do 
           result <- quickCheckWithResult qcconf (forAll gen $ (str2prop prop) filename (prog,args) mencode outdir)
           reduce filename (prog,args) outdir result


process :: (Mutation a, Show a, Arbitrary a)
    => ((a -> BSL.ByteString),(BS.ByteString -> a))
    -> Bool -> String -> FilePath -> String ->
    Int -> Int -> FilePath -> FilePath -> IO Result 

process (mencode,mdecode) par filename cmd prop maxSuccess maxSize outdir seeds =

    let (prog, args) = (Prelude.head spl, Prelude.tail spl)
    in 
      if prop /= "mutate" then (
        process_custom arbitrary (mencode,mdecode) par filename cmd prop maxSuccess maxSize outdir seeds
      ) else (
        if seeds /= "" then (
             do
                 fs <- listDirectory seeds
                 xs <- mapM makeRelativeToCurrentDirectory fs
                 --mapM_ print fs
                 xs <- withCurrentDirectory seeds (mapM (decodeFile mdecode) xs)
                 xs <- return $ Prelude.filter isJust xs
                 xs <- return $ Prelude.map fromJust xs
                 --mapM_ print xs
                 --xs <- return $ map mdecode xs
                 quickCheckWithResult stdArgs { maxSuccess = maxSuccess , maxSize = maxSize, chatty = not par } (noShrinking $ prop_MutateGen filename (prog,args) mencode outdir xs))
                 --undefined )
            else (error "You should specifiy a directory with seeds!")
        )
    
    where spl = splitOn " " cmd



_main fs (MainArgs _ cmd filename prop maxSuccess maxSize outdir seeds b) = process fs b filename cmd prop maxSuccess maxSize outdir seeds
--_main fs (MainArgs _ cmd filename prop coefs_filename maxSuccess maxSize outdir seeds b) = process fs b filename cmd prop coefs_filename maxSuccess maxSize outdir seeds

main fs fargs False = (\x -> (_main fs x) >> return ()) $ fargs ""
main fs fargs True  = processPar fargs (\x -> (_main fs x) >> return ())


#ifdef NET

--netprocess :: (Show a, Arbitrary a) => (a -> ByteString) -> Bool -> FilePath -> String -> String -> Int -> Int -> FilePath -> IO Result 
netprocess mencode par _ host prop maxSuccess maxSize outdir _ =
    --let (prog, args) = (Prelude.head spl, Prelude.tail spl)
    case prop of
        "serve" -> quickCheckWithResult stdArgs { maxSuccess = maxSuccess , maxSize = maxSize, chatty = not par } (noShrinking $ serveprop 6055 [] mencode)

        "connect" -> quickCheckWithResult stdArgs { maxSuccess = maxSuccess , maxSize = maxSize, chatty = not par } (noShrinking $ cconnectprop 6055 host mencode)


        _     -> error "Invalid action selected"

_netmain mencode (MainArgs _ cmd filename prop maxSuccess maxSize outdir seeds b) = netprocess mencode b filename cmd prop maxSuccess maxSize outdir seeds

netmain mencode fargs False = (\x -> (_netmain mencode x) >> return ()) $ fargs ""
netmain mencode fargs True  = processPar fargs (\x -> (_netmain mencode x) >> return ())

#endif
