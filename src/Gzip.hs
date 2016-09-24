{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Gzip where


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


data FLG = FLG {
               ftext :: Bool,
               fhcrc :: Bool,
               fextra :: Bool,
               fname :: Bool,
               fcomment :: Bool
           }
            deriving(Show,Eq)

data MGzipFile = GZIP { 
                   flg :: FLG,
                   mtime :: Word32,
                   xfl :: ExtraFlag,
                   os :: FileSystem,
                   extras :: ExtraBlock,
                   compressedBlocks :: [Word8],
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
                    fileComment :: String
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

data ExtraFlag = SlowestAlgorithmCompression
               | FastestAlgorithmCompression
               deriving(Enum,Show,Eq)

class CustomEnum a where
        enum :: a -> Word8

instance CustomEnum FileSystem where                  
        enum UNKNOWN = 255
        enum x = fromIntegral $ fromEnum $ x        

instance CustomEnum ExtraFlag where
        enum SlowestAlgorithmCompression = 2
        enum FastestAlgorithmCompression = 4
                                   
putList :: [Word8] -> Put
putList [] = return ()
putList (w:words) = do putWord8 w
                       putList words
instance Binary FLG where
        put flg = putWord8 $ packWord8LE (ftext $ flg) (fhcrc $ flg) (fextra $ flg) (fname $ flg) (fcomment $ flg) False False False 
        get     = undefined

instance Binary MGzipFile where
        put gzip = do putWord8 $ 31 
                      putWord8 $ 139
                      putWord8 $ 8 
                      put $ flg $ gzip
                      putWord32le $ mtime gzip 
                      putWord8 $ enum $ xfl gzip
                      putWord8 $ enum $ os gzip 
                      when (fextra $ flg gzip) $ do putWord16le $ fromIntegral $ xlen $ extras gzip
                                                    putByteString $ C.pack $ ([(fst $ ident $ extras gzip)] ++ [(snd $ ident $ extras gzip)])
                                                    putWord16le $ fromIntegral $ (xlen $ extras gzip) - 4
                                                    putByteString $ C.pack $ subfield $ extras gzip
                      when (fname $ flg gzip) $ putByteString $ C.pack $ (fileName $ extras gzip) ++ "\0"
                      when (fcomment $ flg gzip) $ putByteString $ C.pack $ (fileComment $ extras gzip) ++ "\0"
                      when (fhcrc $ flg gzip) $ do putWord8 $ sel1 $ crc32 gzip
                                                   putWord8 $ sel2 $ crc32 gzip 
                      putList $ compressedBlocks gzip
                      putWord8 $ sel1 $ crc32 gzip
                      putWord8 $ sel2 $ crc32 gzip
                      putWord8 $ sel3 $ crc32 gzip
                      putWord8 $ sel4 $ crc32 gzip
                      putWord32le $ fromIntegral $ isize gzip        
        get = undefined


$(devArbitrary ''MGzipFile)

mencode :: MGzipFile -> L.ByteString
mencode = encode


