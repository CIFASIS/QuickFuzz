{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module Http where
import Args
import Test.QuickCheck
import Check

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.ByteString as B

import Network.HTTP.Base
import Network.HTTP.Headers
import DeriveArbitrary
import Data.DeriveTH
import Data.Char (ord, chr)
import Data.List.Split

import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T

--genName :: Gen String
--genName = listOf1 validChars :: Gen String
--  where validChars = chr <$> choose (97, 122)

--instance Arbitrary String where
--   arbitrary = genName

instance Arbitrary a => Arbitrary (Response a) where
   arbitrary = do 
     c <- arbitrary
     r <- arbitrary
     h <- arbitrary
     x <- arbitrary 
     return $ Response {rspCode = c, rspReason = r, rspHeaders = h, rspBody = x }

instance Arbitrary ResponseCode where
   arbitrary = do 
     a <- elements [1,2,3,4,5]
     b <- elements [0,1,2,3,4,5,6,7,8,9]
     c <- elements [0,1,2,3,4,5,6,7,8,9]
     --return (3,0,1)
     return (a,b,c)
 
data MResponse = MR [Response String] deriving ( Show )

instance Arbitrary MResponse where
  arbitrary = do 
      xs <- infiniteListOf (resize 100 (arbitrary))  --vectorOf 10000000 (resize 100 (arbitrary))
      return $ MR xs

derive makeArbitrary ''Header
derive makeArbitrary ''HeaderName

packStr :: String -> B.ByteString
packStr =  encodeUtf8 . T.pack -- B.pack . Prelude.map (fromIntegral . ord) . Prelude.filter

mencode :: MResponse -> [B.ByteString]
mencode (MR x) = Prelude.map (packStr . show) x

httpmain (MainArgs _ cmd filename prop maxSuccess maxSize outdir b) = let (prog, args) = (Prelude.head spl, Prelude.tail spl) in
    (case prop of
        "serve" -> quickCheckWith stdArgs {chatty = True, maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ serveprop filename 5002 [] mencode)
        _     -> error "Invalid action selected"
    ) where spl = splitOn " " cmd

main fargs False = httpmain $ fargs ""
main fargs True  = processPar fargs httpmain
