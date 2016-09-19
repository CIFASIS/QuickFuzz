{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

module Gzip1 where

--import Data.Binary( Binary(..), encode )

import qualified Data.ByteString.Lazy as L


import Data.Bits.Bitwise
import Data.ByteString
import Data.Binary.Put
import Data.Binary
import Data.Tuple.Select


curry8 :: ((a,b,c,d,e,f,g,h) -> i) -> a -> b -> c -> d -> f -> g -> h -> i
curry8 f x1 x2 x3 x4 x5 x6 x7 x8 = f (x1,x2,x3,x4,x5,x6,x7,x8)

uncurry8 :: (a -> b -> c -> d -> e -> f -> g -> h -> i) -> (a,b,c,d,e,f,g,h) -> i)
uncurry8 f p = f (sel1 p) (sel2 p) (sel3 p) (sel4 p) (sel5 p) (sel6 p) (sel7 p) (sel8 p)
 
--type MGzipFile  = (CompressParams,L.ByteString)
type Byte = (Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool) -- Se lee de izquierda a derecha, ejemplo, el 01000 es el 2
data MGzipFile = GZIP { 
		   cm :: Byte,
		   flg :: Byte,
		   mtime :: (Byte,Byte,Byte,Byte)
		   xfl :: Byte,
		   crc32 :: (Byte,Byte,Byte,Byte)
		   extras
		   isize ::
		 }

data ExtraFlags = XFL {
		    xlen :: Maybe Byte
		    crc16 :: Maybe (Byte,Byte)
    
instance Binary MGzipFile where
	put gzip = do putWord8 $ 31 -- id1
		      putWord8 $ 139
		      putWord8 $ uncurry8 $ packWord8BE $ cm
		      putWord8 $ uncurry8 $ packWord8BE $ flg
		      putWord8 $ uncurry8 $ packWord8BE $ sel1 $ mtime
		      putWord8 $ uncurry8 $ packWord8BE $ sel2 $ mtime
		      putWord8 $ uncurry8 $ packWord8BE $ sel3 $ mtime
		      putWord8 $ uncurry8 $ packWord8BE $ sel4 $ mtime
		      putWord8 $ uncurry8 $ packWord8BE $ xfl
		      putWord8 $ 3 --at the moment, in unix
		      case flg of
			 (_,_,True,_,_,_,_,_) -> 
--mencode :: MGzipFile -> L.ByteString
--mencode (p,bs) = compressWith p bs
