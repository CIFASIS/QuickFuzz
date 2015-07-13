{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

import Test.QuickCheck
import Test.QuickCheck.Instances
import Test.QuickCheck.Gen

import Control.Monad.Zip
import Control.Exception
import Data.Binary( Binary(..), encode )

import Text.XML.HaXml.Types
import Text.XML.HaXml.Pretty

import Text.PrettyPrint.HughesPJ  (render)

import qualified Data.ByteString.Lazy as L
import Data.DeriveTH
import Data.Word(Word8, Word16, Word32)
import Data.Int( Int16, Int8 )

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS

import System.Process
import System.Exit
import System.Posix

getFileSize :: String -> IO FileOffset
getFileSize path = do
    stat <- getFileStatus path
    return (fileSize stat)

instance Arbitrary (V.Vector Word32) where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Word32)
     return $ V.fromList l

instance Arbitrary (V.Vector Word16) where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Word16)
     return $ V.fromList l

instance Arbitrary (VU.Vector Int) where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Int)
     return $ VU.fromList l

instance Arbitrary (VU.Vector Int8) where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Int8)
     return $ VU.fromList l

instance Arbitrary (VU.Vector Word8) where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Word8)
     return $ VU.fromList l

instance Arbitrary (VS.Vector Word16) where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Word16)
     return $ VS.fromList l

instance Arbitrary (V.Vector (VU.Vector Word8)) where
   arbitrary = do 
     l <- listOf (arbitrary :: Gen Word8)
     return $ V.replicate 32 (VU.fromList l)

instance Arbitrary String where
    arbitrary = return "a" --vectorOf 1 (oneof $ map return "a")

derive makeArbitrary ''Document
derive makeArbitrary ''Misc
derive makeArbitrary ''Element
derive makeArbitrary ''EntityDef
derive makeArbitrary ''Content
derive makeArbitrary ''Prolog
derive makeArbitrary ''AttValue
derive makeArbitrary ''NDataDecl
derive makeArbitrary ''Reference
derive makeArbitrary ''ExternalID
derive makeArbitrary ''QName
derive makeArbitrary ''DocTypeDecl
derive makeArbitrary ''EntityValue
derive makeArbitrary ''XMLDecl
derive makeArbitrary ''MarkupDecl
derive makeArbitrary ''Namespace
derive makeArbitrary ''PubidLiteral
derive makeArbitrary ''EV
derive makeArbitrary ''NotationDecl
derive makeArbitrary ''EncodingDecl
derive makeArbitrary ''SystemLiteral
derive makeArbitrary ''EntityDecl
derive makeArbitrary ''PublicID
derive makeArbitrary ''AttListDecl
derive makeArbitrary ''PEDecl


derive makeArbitrary ''ElementDecl
derive makeArbitrary ''PEDef
derive makeArbitrary ''AttDef
derive makeArbitrary ''GEDecl
derive makeArbitrary ''ContentSpec
derive makeArbitrary ''DefaultDecl
derive makeArbitrary ''AttType
derive makeArbitrary ''CP
derive makeArbitrary ''FIXED

derive makeArbitrary ''Mixed
derive makeArbitrary ''EnumeratedType
derive makeArbitrary ''Modifier
derive makeArbitrary ''TokenizedType

filenames = take 1 (repeat "buggy.xml")

handler :: SomeException -> IO ()
handler _ = return ()
 
prop = do
  zips  <- sample' (resize 3 (arbitrary :: Gen (Document ())))
  pid <- getProcessID 
  mapM_ (\(filename,zipf) -> 
      do
       --putStrLn "Writting file.."
       catch (writeFile (filename ++ "." ++ (show pid)) ((render . document) zipf)) handler
       --putStrLn "Executing.."
       --ret <- rawSystem "/home/vagrant/.local/bin/honggfuzz" ["-q", "-N3000", "-f", "buggy.dot", "--", "/usr/bin/dot","___FILE___"]
       --putStrLn (renderDot zipf)
       ret <- rawSystem "/usr/bin/zzuf" ["-s", "0:1", "-q", "-R0", "-c", "-S", "-T", "3", "/usr/bin/xmllint","buggy.xml"]
       --ret <- rawSystem "/usr/bin/valgrind" ["--log-file=xml.log","--quiet","/usr/bin/xmllint","buggy.xml"]
       --size <- getFileSize "xml.log"
       --if size > 0 then exitWith (ExitFailure 1) else return ()
       case ret of
        ExitFailure x -> ( do 
                            putStrLn (show x) 
                            exitWith ret)
        _             -> return ()
 
     ) (zip filenames zips)
  prop

main = prop
