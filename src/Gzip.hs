{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Gzip where

--import Data.Binary( Binary(..), encode )

import qualified Data.ByteString.Lazy as L


import Data.Bits.Bitwise
import Data.Bits as B
import Data.ByteString
import Data.ByteString.Char8 as C
import Data.Binary.Put
import Data.Binary
import Data.Tuple.Select
import Data.Time.Clock as T
import Control.Monad
import Data.Int

import Test.QuickCheck
import DeriveArbitrary 

--type MGzipFile  = (CompressParams,L.ByteString)
data MGzipFile = GZIP { 
		   flg :: Word8,
		   mtime :: Word32,
		   xfl :: Word8,
		   os :: FileSystem,
		   extras :: ExtraBlock,
		   crc32 :: (Word8,Word8,Word8,Word8),
		   isize :: Int32
		 }
		deriving(Show,Eq)

data ExtraBlock = EB {
		    xlen :: Int16,
		    ident :: (Char,Char),
		    len :: Int16, 
		    subfield :: String,
		    fileName :: String,
		    fileComment :: String,
		    compressedBlocks :: [Word8]
		   }
		deriving(Show,Eq)

data FileSystem = Fat
		| Amiga
		| VMS
		| Unix
		| CMS
		| AtariTOS
		| HPFS
		| Macintosh
		| ZSystem
		| CPM
		| TOPS20
		| NTFS
		| QDOS
		| AcornRISCOS
		| UNKNOWN
		deriving(Enum,Show,Eq)
		  
enum :: FileSystem -> Word8
enum UNKNOWN = 255
enum x = fromIntegral $ fromEnum $ x	
				   
putList :: [Word8] -> Put
putList [] = return ()
putList (w:words) = do putWord8 w
		       putList words
 
instance Binary MGzipFile where
	put gzip = do putWord8 $ 31 
		      putWord8 $ 139
		      putWord8 $ 8 
		      putWord8 $ flg gzip
		      putWord32le $ mtime gzip 
		      putWord8 $ xfl gzip
		      putWord8 $ enum $ os gzip 
{- if FEXTRA -}	      when (sel3 $ unpackWord8LE $ flg gzip) $ do putWord16le $ fromIntegral $ xlen $ extras gzip
							          putByteString $ C.pack $ ([(fst $ ident $ extras gzip)] ++ [(snd $ ident $ extras gzip)])
								  putWord16le $ fromIntegral $ (xlen $ extras gzip) - 32
								  putByteString $ C.pack $ subfield $ extras gzip
{- if FNAME -}	      when (sel4 $ unpackWord8LE $ flg gzip) $ putByteString $ C.pack $ (fileName $ extras gzip) ++ "\0"
{- if FCOMMENT -}     when (sel5 $ unpackWord8LE $ flg gzip) $ putByteString $ C.pack $ (fileComment $ extras gzip) ++ "\0"
{- if FHCRC -}        when (sel2 $ unpackWord8LE $ flg gzip) $ do putWord8 $ sel1 $ crc32 gzip
								  putWord8 $ sel2 $ crc32 gzip 
								  putList $ compressedBlocks $ extras gzip
		      putWord8 $ sel1 $ crc32 gzip
		      putWord8 $ sel2 $ crc32 gzip
		      putWord8 $ sel3 $ crc32 gzip
		      putWord8 $ sel4 $ crc32 gzip
		      putWord32le $ fromIntegral $ isize gzip	
	get = undefined


$(devArbitrary ''MGzipFile)

mencode :: MGzipFile -> L.ByteString
mencode = encode


