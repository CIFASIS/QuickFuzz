{-# LANGUAGE TemplateHaskell, FlexibleInstances#-}

import Test.QuickCheck
import Test.QuickCheck.Instances

import Control.Monad.Zip
import Control.Exception
import Data.Binary( Binary(..), encode )

import Language.Dot.Syntax
import Language.Dot.Pretty

import qualified Data.ByteString.Lazy as L
import Data.DeriveTH
import Data.Word(Word8, Word16, Word32)
import Data.Int( Int16, Int8 )

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS

import System.Process
import System.Exit


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



derive makeArbitrary ''Graph
derive makeArbitrary ''Statement
derive makeArbitrary ''Subgraph
derive makeArbitrary ''Id
derive makeArbitrary ''NodeId
derive makeArbitrary ''GraphDirectedness
derive makeArbitrary ''AttributeStatementType
derive makeArbitrary ''Xml
derive makeArbitrary ''Entity
derive makeArbitrary ''GraphStrictness
derive makeArbitrary ''XmlAttribute

derive makeArbitrary ''XmlAttributeValue
derive makeArbitrary ''XmlName
derive makeArbitrary ''EdgeType
derive makeArbitrary ''Attribute
derive makeArbitrary ''Port
derive makeArbitrary ''Compass

{-
derive makeArbitrary ''ExifData
derive makeArbitrary ''JpgAdobeApp14
derive makeArbitrary ''JFifUnit
derive makeArbitrary ''AdobeTransform

derive makeArbitrary ''JpgJFIFApp0
derive makeArbitrary ''ImageFileDirectory


derive makeArbitrary ''DctComponent
derive makeArbitrary ''JpgQuantTableSpec
derive makeArbitrary ''JpgHuffmanTableSpec
derive makeArbitrary ''JpgScanSpecification
derive makeArbitrary ''JpgScanHeader
derive makeArbitrary ''JpgFrameKind
derive makeArbitrary ''JpgComponent
derive makeArbitrary ''JpgFrameHeader
derive makeArbitrary ''JpgFrame
derive makeArbitrary ''JpgImage
-}

filenames = take 3 (repeat "buggy.dot")

handler :: SomeException -> IO ()
handler _ = return ()
 
prop = do
  zips  <- sample' (arbitrary :: Gen Graph)
  mapM_ (\(filename,zipf) -> 
      do
       catch (writeFile filename (renderDot zipf)) handler
       ret <- rawSystem "/usr/bin/zzuf" ["-s", "0:10", "-c", "-S", "-q" ,"-T", "3", "/usr/bin/dot","buggy.dot"]
       case ret of
        ExitFailure x -> ( do 
                            putStrLn (show x) 
                            exitWith ret)
        _             -> return ()
 
     ) (zip filenames zips)
  prop

main = prop
