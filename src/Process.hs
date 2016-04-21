{-# LANGUAGE CPP                #-}

module Process where

import Args
import Mutation
import Test.QuickCheck
import Check
import Data.List.Split
import Data.Maybe
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS

--import System.FilePath
import System.Directory hiding (listDirectory, withCurrentDirectory)

#ifdef NET

import Network.Socket

#endif

import Control.Exception --( bracket, bracketOnError, evaluate, catch )
import Exceptions

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


--mhandler :: SomeException -> Maybe a
--mhandler x = return Nothing

--decodeFile :: (
decodeFile mdecode filename =
  do
    x <- BS.readFile filename
    --print (BS.length x)
    --print filename
    x <- catch (evaluate $ Just $ mdecode x) dec_handler
    return x

process_custom :: Show a
    => Gen a -> ((a -> BSL.ByteString),(BS.ByteString -> a)) 
    -> Bool -> FilePath -> String -> String -> Int
    -> Int -> FilePath -> FilePath -> IO Result 
process_custom gen (mencode,mdecode) par filename cmd prop maxSuccess maxSize outdir seeds =
    let (prog, args) = (Prelude.head spl, Prelude.tail spl)
    in (case prop of
        "zzuf" ->
            do  createDirectoryIfMissing True outdir
                quickCheckWithResult stdArgs { maxSuccess = maxSuccess , maxSize = maxSize, chatty = not par } (noShrinking $ forAll gen $ zzufprop filename prog args mencode outdir)
        "radamsa" ->
            do  createDirectoryIfMissing True outdir
                quickCheckWithResult stdArgs { maxSuccess = maxSuccess , maxSize = maxSize, chatty = not par } (noShrinking $ forAll gen $ radamprop filename prog args mencode outdir)
        "check" ->
            do  createDirectoryIfMissing True outdir
                quickCheckWithResult stdArgs { maxSuccess = maxSuccess , maxSize = maxSize, chatty = not par } (noShrinking $ forAll gen $ checkprop filename prog args mencode outdir)
        "gen" ->
            do  createDirectoryIfMissing True outdir
                quickCheckWithResult stdArgs { maxSuccess = maxSuccess , maxSize = maxSize, chatty = not par } (noShrinking $ forAll gen $ genprop filename prog args mencode outdir)
    ) where spl = splitOn " " cmd


process :: (Mutation a, Show a, Arbitrary a)
    => ((a -> BSL.ByteString),(BS.ByteString -> a))
    -> Bool -> FilePath -> String -> String ->
    Int -> Int -> FilePath -> FilePath -> IO Result 
process (mencode,mdecode) par filename cmd prop maxSuccess maxSize outdir seeds =  
    let (prog, args) = (Prelude.head spl, Prelude.tail spl)
    in (case prop of
        "mut" ->
            if seeds /= "" then (
             do
                 fs <- listDirectory seeds
                 xs <- mapM makeRelativeToCurrentDirectory fs
                 --mapM_ print fs
                 xs <- withCurrentDirectory seeds (mapM (decodeFile mdecode) xs)
                 xs <- return $ Prelude.filter isJust xs
                 xs <- return $ Prelude.map fromJust xs
                 createDirectoryIfMissing True outdir

                 --mapM_ print xs
                 --xs <- return $ map mdecode xs
                 quickCheckWithResult stdArgs { maxSuccess = maxSuccess , maxSize = maxSize, chatty = not par } (noShrinking $ mutprop filename prog args mencode outdir maxSize xs))
            else (error "You should specifiy a directory with seeds!")
        "exec" ->
            do  createDirectoryIfMissing True outdir
                quickCheckWithResult stdArgs { maxSuccess = maxSuccess , maxSize = maxSize, chatty = not par } (noShrinking $ execprop filename prog args mencode outdir)
        "honggfuzz" ->
            do  createDirectoryIfMissing True outdir
                quickCheckWithResult stdArgs { maxSuccess = maxSuccess , maxSize = maxSize, chatty = not par } (noShrinking $ honggprop filename prog args mencode outdir)

        _     -> process_custom arbitrary (mencode,mdecode) par filename cmd prop maxSuccess maxSize outdir seeds
    
    ) where spl = splitOn " " cmd

_main fs (MainArgs _ cmd filename prop maxSuccess maxSize outdir seeds b) = process fs b filename cmd prop maxSuccess maxSize outdir seeds

main fs fargs False = (\x -> (_main fs x) >> return ()) $ fargs ""
main fs fargs True  = processPar fargs (\x -> (_main fs x) >> return ())


#ifdef NET

--netprocess :: (Show a, Arbitrary a) => (a -> ByteString) -> Bool -> FilePath -> String -> String -> Int -> Int -> FilePath -> IO Result 
netprocess mencode par _ host prop maxSuccess maxSize outdir _ =
    --let (prog, args) = (Prelude.head spl, Prelude.tail spl)
    case prop of
        "serve" ->
            do  createDirectoryIfMissing True outdir
                quickCheckWithResult stdArgs { maxSuccess = maxSuccess , maxSize = maxSize, chatty = not par } (noShrinking $ serveprop 6055 [] mencode)

        "connect" -> 
            do  createDirectoryIfMissing True outdir
              quickCheckWithResult stdArgs { maxSuccess = maxSuccess , maxSize = maxSize, chatty = not par } (noShrinking $ cconnectprop 6055 host mencode)


        _     -> error "Invalid action selected"

_netmain mencode (MainArgs _ cmd filename prop maxSuccess maxSize outdir seeds b) = netprocess mencode b filename cmd prop maxSuccess maxSize outdir seeds

netmain mencode fargs False = (\x -> (_netmain mencode x) >> return ()) $ fargs ""
netmain mencode fargs True  = processPar fargs (\x -> (_netmain mencode x) >> return ())

#endif
