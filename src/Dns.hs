{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module Dns where
import Test.QuickCheck

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as L8
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as IB (c2w, w2c)

import Data.IP

import Network.DNS.Types
import Network.DNS.Encode
import DeriveArbitrary
import Data.DeriveTH
import Data.Char (ord, chr)
import Data.List.Split

import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T

--genName :: Gen String
--genName = listOf1 validChars :: Gen String
--  where validChars = chr <$> choose (97, 122)

--instance Arbitrary String where
--   arbitrary = do 
--      x <- genName
--      y <- genName
--      return $ x ++ "=" ++ y 

import Data.Word(Word8, Word16, Word32)
import Data.Int( Int16, Int8 )

--derive makeArbitrary ''TYPE
--derive makeArbitrary ''IPv6

$(devArbitrary ''DNSMessage)

convertL8 :: LC8.ByteString -> B.ByteString
convertL8 =  B.pack . (map IB.c2w) .  LC8.unpack --L8.pack . (Prelude.map IB.w2c) . B.unpack

data MMessage = MM [DNSMessage] deriving ( Show )

instance Arbitrary MMessage where
  arbitrary = do 
      xs <- infiniteListOf (resize 10 (arbitrary))
      return $ MM xs

mencode (MM x) = Prelude.map (convertL8 . encode) x
--mencode (MM x) = Prelude.map encode x
{-
tftpmain (MainArgs _ cmd filename prop maxSuccess maxSize outdir b) = let (prog, args) = (Prelude.head spl, Prelude.tail spl) in
    (case prop of
        "serve" -> quickCheckWith stdArgs {chatty = True, maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ serveprop filename 8099 [] mencode)
        _     -> error "Invalid action selected"
    ) where spl = splitOn " " cmd

main fargs False = tftpmain $ fargs ""
main fargs True  = processPar fargs tftpmain
-}
