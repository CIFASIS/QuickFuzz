{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}


module Gzip where

--import Data.Binary( Binary(..), encode )

import qualified Data.ByteString.Lazy as L


import Data.Bits.Bitwise
import Data.ByteString
import Data.Binary.Put
import Data.Binary
import Data.Tuple.Select
import Data.Time.Clock as T
import Control.Monad


--type MGzipFile  = (CompressParams,L.ByteString)
data MGzipFile = GZIP { 
		   cm :: Word8,
		   flg :: Word8,
		   --mtime :: T.UTCTime,
		   xfl :: Word8,
		   os :: Word8,
		   extras :: ExtraBlock,
		   crc32 :: Word32,
		   isize :: Word32
		 }
		deriving(Show,Eq)

data ExtraBlock = EB {
		    xlen :: Word8,
		    xlenExtra :: [Word8],
		    fileName :: String,
		    fileComment :: [Word8],
		    crc16 :: Word16,
		    compressedBlocks :: [Word8]
		  }
		deriving(Show,Eq)				   
putList :: [Word8] -> Put
putList [w] = putWord8 w
putList (w1:(w2:words)) = do putWord8 w1
			     putWord8 w2
			     putList words
instance Binary MGzipFile where
	put gzip = do putWord8 $ 31 -- id1
		      putWord8 $ 139
		      putWord8 $ cm gzip
		      putWord8 $ flg gzip
		      --putWord32le $ tencode $ mtime gzip -- revisar tencode
		      putWord8 $ xfl gzip
		      putWord8 $ os gzip --revisar
		      when (sel3 $ unpackWord8LE $ flg gzip) $ do putWord8 $ xlen $ extras gzip
							          putList $ xlenExtra $ extras gzip
		      when (sel4 $ unpackWord8LE $ flg gzip) $ do putWord8 0 -- putWord fileName
		      when (sel5 $ unpackWord8LE $ flg gzip) $ putList $ fileComment $ extras gzip
		      when (sel2 $ unpackWord8LE $ flg gzip) $ do putWord16le $ crc16 $ extras gzip
								  putList $ compressedBlocks $ extras gzip
		      putWord32le $ crc32 gzip
		      putWord32le $ isize gzip	
	get = undefined

tencode :: T.UTCTime -> Word32
tencode t = undefined

mencode :: MGzipFile -> L.ByteString
mencode gzip = undefined
