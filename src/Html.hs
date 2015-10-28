{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module Html where

import Args
import Test.QuickCheck
import Check

--import Control.Monad.Zip
--import Control.Exception
--import Data.Binary( Binary(..), encode )

import Text.XML.HaXml.Types
--import Text.XML.HaXml.ByteStringPP
import Text.XML.HaXml.Html.Generate
import Text.PrettyPrint.HughesPJ

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.DeriveTH
import DeriveArbitrary

import Vector
import ByteString

import Data.Char (chr)
import Data.List.Split

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

type MHtml =  [Content ()]

genName :: Gen String
genName = listOf1 validChars :: Gen String
  where validChars = chr <$> choose (97, 122)

instance Arbitrary String where
   arbitrary = genName

--instance Arbitrary String where
--   arbitrary = oneof $ map return ["a", "b", "href"] 

mencode :: MHtml -> L8.ByteString
mencode x = L8.pack $ render $ htmlprint x

main (MainArgs _ cmd filename prop maxSuccess maxSize outdir _) = let (prog, args) = (head spl, tail spl) in
    (case prop of
        "zzuf" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ zzufprop filename prog args mencode outdir)
        "radamsa" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ radamprop filename prog args mencode outdir)
        "check" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ checkprop filename prog args mencode outdir)
        "gen" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ genprop filename prog args mencode outdir)
        "exec" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ execprop filename prog args mencode outdir)
        _     -> error "Invalid action selected"
    ) where spl = splitOn " " cmd
