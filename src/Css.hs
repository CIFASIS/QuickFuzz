{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module Css where

import Args
import Test.QuickCheck
import Check

import Data.Binary( Binary(..), encode )

import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Char8 as C8
import Data.DeriveTH

import DeriveArbitrary
import ByteString
import Vector

import Text.PrettyPrint

import Language.Css.Syntax
import Language.Css.Pretty
import Language.Css.Build

import Data.Monoid
import Data.List.Split
import Data.Char (chr)
import qualified Data.Text as T

import Linear

type MCssFile  = StyleSheet

genName :: Gen String
genName = listOf1 validChars :: Gen String
  where validChars = chr <$> choose (97, 122)

instance Arbitrary String where
   arbitrary = genName
   --arbitrary = oneof $ Prelude.map return ["a", "b", "c", "d", "e"]


-- $(derive makeArbitrary ''MSvgFile)
derive makeArbitrary ''StyleSheet
-- $(deriveArbitraryRec ''StyleBody)
derive makeArbitrary ''StyleBody
$(deriveArbitraryRec ''AtImport)
$(deriveArbitraryRec ''AtCharSet)
derive makeArbitrary ''RuleSet
--derive makeArbitrary ''Sel
instance Arbitrary Sel where
   arbitrary = do 
                x <- arbitrary
                y <- arbitrary
                z <- arbitrary
                frequency [(10, return (SSel x)), (1, return (DescendSel y z)), (1, return (ChildSel y z)), (1, return (AdjSel y z))]


derive makeArbitrary ''AtPage
$(deriveArbitraryRec ''AtMedia)
derive makeArbitrary ''AtFontFace
derive makeArbitrary ''SimpleSel
derive makeArbitrary ''Decl
derive makeArbitrary ''SubSel
$(deriveArbitraryRec ''Prio)
derive makeArbitrary ''PseudoVal

instance Arbitrary (Language.Css.Syntax.Expr) where
   arbitrary = do 
                x <- arbitrary
                y <- arbitrary
                z <- arbitrary
                frequency [(10, return (EVal x)), (1, return (SlashSep y z)), (1, return (CommaSep y z)), (1, return (SpaceSep y z))]

--   arbitrary = oneof $ map return [aqua, black, blue, gray, green, lime]

-- $(derive makeArbitrary ''Language.Css.Syntax.Expr)
$(deriveArbitraryRec ''Func)
$(deriveArbitraryRec ''Attr)
$(deriveArbitraryRec ''Value)

-- $(derive makeArbitrary ''Transform)

-- encodeMCssFile x = LC8.pack $ "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"300px\" height=\"300px\" viewBox=\"0 0 300 300\"> <style type=\"text/css\">" ++ (render (pretty x)) ++ "</style> </svg>"

encodeMCssFile x = LC8.pack $ (render (pretty x)) 

mencode :: MCssFile -> LC8.ByteString
mencode = encodeMCssFile

