{-# LANGUAGE TemplateHaskell, FlexibleInstances, DeriveAnyClass, OverloadedStrings #-}

module CPIO where

import Test.QuickCheck

import qualified Data.ByteString.Lazy as BL
import Data.ByteString (ByteString)

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as B16

--import           Data.Binary.Get        (getWord32be, runGet)
import           Data.Binary.Put        (putWord32be, runPut)
import           Data.Bits              ((.&.))

import Control.Monad (forM_)
import Data.Binary( Binary(..), encode )
import Data.Word (Word32)

import Vector
import Images
import DeriveArbitrary
import ByteString
import Data.DeriveTH

data Entry = Entry
  { cpioInode    :: !Word32
  , cpioMode     :: !Word32
  , cpioUid      :: !Word32
  , cpioGid      :: !Word32
  , cpioNLink    :: !Word32
  , cpioMTime    :: !Word32
  , cpioFileSize :: !Word32
  , cpioDevMaj   :: !Word32
  , cpioDevMin   :: !Word32
  , cpioRDevMaj  :: !Word32
  , cpioRDevMin  :: !Word32
  , cpioCRC32    :: Maybe Word32
  , cpioFileName :: ByteString
  , cpioFileData :: BL.ByteString
  } deriving (Show, Read, Eq)

data MCPIO = CPIO [Entry] deriving Show

-- $(deriveArbitraryRec ''MCPIO)
derive makeArbitrary ''MCPIO
derive makeArbitrary ''Entry

encodeR32 x = B16.encode $ BS.concat $ BL.toChunks $ runPut (putWord32be x)

trailerText :: ByteString
trailerText = "TRAILER!!!"

alignTo4 :: Integral a => a -> a
alignTo4 0 = 0
alignTo4 n = 3 - ((n - 1) `mod` 4)

instance Binary MCPIO where
  put (CPIO (entry:xs)) = do 
                            put entry
                            put (CPIO xs)
  put (CPIO [])         = put (Entry 0 0 0 0 0 0 0 0 0 0 0 (Just 0) trailerText "")

  get = undefined 

instance Binary Entry where
  put entry = do
               case cpioCRC32 entry of
                 Nothing -> put ("070701" :: ByteString)
                 Just _ -> put ("070702" :: ByteString)
               put $ encodeR32 $ cpioInode entry
               put $ encodeR32 $ cpioMode entry
               put $ encodeR32 $ cpioUid entry
               put $ encodeR32 $ cpioGid entry
               put $ encodeR32 $ cpioNLink entry
               put $ encodeR32 $ cpioMTime entry
               let file_size = cpioFileSize entry
               put $ encodeR32 $ file_size
               put $ encodeR32 $ cpioDevMaj entry
               put $ encodeR32 $ cpioDevMin entry
               put $ encodeR32 $ cpioRDevMaj entry
               put $ encodeR32 $ cpioRDevMin entry
               let filename_length = 1 + (fromInteger $ toInteger $ BS.length $ cpioFileName entry)
               put $ encodeR32 $ (filename_length :: Word32)
               case cpioCRC32 entry of
                 Nothing -> put $ encodeR32 0
                 Just x -> put $ encodeR32 x
               put $ cpioFileName entry
               put $ ("\NUL" :: ByteString)
               put $ BS.replicate ((alignTo4 $ fromInteger (110 + (toInteger filename_length))) :: Int)  0
               forM_ (BL.toChunks $ cpioFileData entry) put
               put $ BS.replicate (alignTo4 (fromInteger $ toInteger file_size)) 0
             
  get = undefined

{-
write_entry entry = do
      case cpioCRC32 entry of
        Nothing -> "070701"
        Just _ -> "070702"
      encodeR32 $ cpioInode entry
      encodeR32 $ cpioMode entry
      encodeR32 $ cpioUid entry
      encodeR32 $ cpioGid entry
      encodeR32 $ cpioNLink entry
      encodeR32 $ cpioMTime entry
      let file_size = cpioFileSize entry
      encodeR32 $ file_size
      encodeR32 $ cpioDevMaj entry
      encodeR32 $ cpioDevMin entry
      encodeR32 $ cpioRDevMaj entry
      encodeR32 $ cpioRDevMin entry
      let filename_length =
            1 + (fromInteger $ toInteger $ BS.length $ cpioFileName entry)
      encodeR32 $ (filename_length :: Word32)
      case cpioCRC32 entry of
        Nothing -> encodeR32 0
        Just x -> encodeR32 x
      yield $ cpioFileName entry
      yield $ "\NUL"
      yield $ BS.replicate (alignTo4 $ 110 + filename_length) 0
      forM_ (BL.toChunks $ cpioFileData entry) yield
      yield $ BS.replicate (alignTo4 (fromInteger $ toInteger file_size)) 0
-}

encodeCPIO :: MCPIO -> BL.ByteString
encodeCPIO = encode

mencode :: MCPIO -> BL.ByteString
mencode = encodeCPIO
