{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module Net where

import Test.QuickCheck
import Network.HTTP.Base
import Network.HTTP.Headers
import Network.DNS.Types
import Network.DNS.Encode
import Data.IP

import Data.Word(Word8, Word16, Word32)
import qualified Data.ByteString as B

instance Arbitrary Domain where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Word8)
     return $ B.pack l

instance Arbitrary IPv4 where
   arbitrary = do
     --vs <- vectorOf 4 (arbitrary :: Gen Int)
     return $ toIPv4 [1,2,3,4]

instance Arbitrary IPv6 where
   arbitrary = do
     --vs <- vectorOf 4 (arbitrary :: Gen Int)
     return $ toIPv6 [0x2001,0xDB8,0,0,0,0,0,1]


--instance Arbitrary ResponseCode where
--   arbitrary = do 
--     a <- elements [1,2,3,4,5]
--     b <- elements [0,1,2,3,4,5,6,7,8,9]
--     c <- elements [0,1,2,3,4,5,6,7,8,9]
--     --return (2,0,0)
--     return (a,b,c)

