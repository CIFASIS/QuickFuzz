{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}

module Utils.Decoding where

import Data.Maybe
import Data.ByteString.Lazy

import Control.DeepSeq
import Control.Monad

import System.Random (randomIO)
import Test.QuickCheck.Random (mkQCGen)
import Test.QuickCheck.Gen

import Test.QuickFuzz.Gen.FormatInfo

import Args
import Debug
import Exception

import System.Directory hiding (listDirectory, withCurrentDirectory)
import Control.Exception ( bracket )

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

decodeFile decf file = do
    !buf <- Data.ByteString.Lazy.readFile file
    forceEvaluation (decf buf)

-- Generate a reproductible value, retrying when the generated value fails to
-- enconde
strictDecode :: (Show base, NFData base) => QFCommand -> FormatInfo base actions 
               -> IO [base]
strictDecode cmd fmt = do
    fs <- listDirectory (inDir cmd)
    xs <- mapM makeRelativeToCurrentDirectory fs
    bsnfs <- withCurrentDirectory (inDir cmd) (mapM (decodeFile (decode fmt)) xs)

    --xs <- withCurrentDirectory (inDir cmd) (mapM (Data.ByteString.Lazy.readFile) xs)
    --bsnfs <- mapM (\x -> forceEvaluation ((decode fmt) x)) xs
    bsnfs <- return $ Prelude.filter isJust bsnfs
    bsnfs <- return $ Prelude.map fromJust bsnfs
    Prelude.putStrLn $ "Loaded " ++ (show (Prelude.length bsnfs)) ++ " of " ++ (show (Prelude.length fs)) ++ " files to mutate."
    return bsnfs


