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
--import DeriveArbitrary
import Data.DeriveTH
import Data.Char (ord, chr)
import Data.List.Split

genName :: Gen String
genName = listOf1 validChars :: Gen String
  where validChars = chr <$> choose (97, 122)

instance Arbitrary String where
   arbitrary = genName


instance Arbitrary a => Arbitrary (Response a) where
   arbitrary = do 
     c <- arbitrary
     r <- arbitrary
     h <- arbitrary
     x <- arbitrary 
     return $ Response {rspCode = c, rspReason = r, rspHeaders = h, rspBody = x }


instance Arbitrary ResponseCode where
   arbitrary = do 
     a <- arbitrary
     b <- arbitrary
     c <- arbitrary
     return (a,b,c)
 
instance Arbitrary MResponse where
  arbitrary = undefined -- vectorOf 1000 (arbitrary)

type MResponse = [Response String]

derive makeArbitrary ''Header
derive makeArbitrary ''HeaderName

--derive makeArbitrary ''ResponseCode
--derive makeArbitrary ''MResponse

-- $(deriveArbitraryRec ''MResponse)

packStr :: String -> B.ByteString
packStr = B.pack . Prelude.map (fromIntegral . ord)

mencode :: MResponse -> B.ByteString
mencode x = packStr (show x)

httpmain (MainArgs _ cmd filename prop maxSuccess maxSize outdir b) = let (prog, args) = (Prelude.head spl, Prelude.tail spl) in
    (case prop of
        "serve" -> quickCheckWith stdArgs {chatty = not b, maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ serveprop filename 5002 [] mencode)
        _     -> error "Invalid action selected"
    ) where spl = splitOn " " cmd

main fargs False = httpmain $ fargs ""
main fargs True  = processPar fargs httpmain
