{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

import Test.QuickCheck
import Test.QuickCheck.Instances
import Test.QuickCheck.Gen

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

import System.Random
  ( RandomGen(..)
  , Random(..)
  , StdGen
  , newStdGen
  )

import Control.Monad
  ( liftM
  )

import Control.Monad.Reader()



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

--instance Arbitrary String where
--    arbitrary = vectorOf 5 (oneof $ map return "abc")

derive makeArbitrary ''Graph
derive makeArbitrary ''Statement
derive makeArbitrary ''Subgraph
--derive makeArbitrary ''Id
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

instance Arbitrary Id where
    arbitrary = do
      s <- (arbitrary :: Gen String)
      i <- (arbitrary :: Gen Integer)
      x <- (arbitrary :: Gen Xml)
      oneof $ map return ([NameId s, StringId s, IntegerId i, FloatId 1.0, XmlId x])

filenames = take 1 (repeat "buggy.txt")

handler :: SomeException -> IO ()
handler _ = return ()
 
prop = do
  zips  <- sample' (resize 50000 (arbitrary :: Gen String))
  mapM_ (\(filename,zipf) -> 
      do
       catch (writeFile filename zipf) handler
       --ret <- rawSystem "/home/vagrant/.local/bin/honggfuzz" ["-q", "-N3000", "-f", "buggy.dot", "--", "/usr/bin/dot","___FILE___"]
       --putStrLn (renderDot zipf)
       --ret <- rawSystem "/usr/bin/zzuf" ["-s", "0:1", "-r0.0", "-q", "-c", "-S", "-I", "x","-T", "3", "-j", "4", "/usr/lib/cups/filter/texttopdf", "", "", "", "1", "", "buggy.txt"]
       ret <- rawSystem "/usr/bin/valgrind" ["--quiet",  "/usr/lib/cups/filter/texttopdf", "", "", "", "1", "", "buggy.txt"] 
       case ret of
        ExitFailure x -> ( do 
                            putStrLn (show x) 
                            exitWith ret)
        _             -> return ()
 
     ) (zip filenames zips)
  prop

main = prop
