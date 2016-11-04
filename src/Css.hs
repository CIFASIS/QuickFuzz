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
import DeriveMArbitrary

import ByteString
import Vector

import Text.PrettyPrint

import Language.Css.Syntax
import Language.Css.Pretty
import Language.Css.Build
import Language.Css.Build.Attributes
import Language.Css.Build.Pseudos

import Data.Monoid
import Data.List.Split
import Data.Char (chr)

import Mutation
import DeriveMutation

import Strings
import Linear

type MCssFile  = StyleSheet

instance Arbitrary String where
   arbitrary = mgenName

-- $(devActions ["Language.Css.Build", "Language.Css.Syntax"] ''Expr False [])
-- $(devArbitrary ''ExprAction)
-- $(devArbitraryWithActions False ''Expr)

$(devActions["Language.Css.Build", "Language.Css.Build.Attributes", "Language.Css.Syntax" ] ''Attr False [''Attr])
$(devArbitrary ''AttrAction)
$(devArbitraryWithActions False ''Attr)

-- $(devActions ["Language.Css.Build.Attributes", "Language.Css.Syntax"] ''AttrIdent False [''AttrIdent])
-- $(devArbitrary ''AttrIdentAction)
-- $(devArbitraryWithActions False ''AttrIdent)

$(devActions ["Language.Css.Build.Pseudos", "Language.Css.Syntax"] ''PseudoVal False [])
$(devArbitrary ''PseudoValAction)
$(devArbitraryWithActions False ''PseudoVal)
 

$(devActions ["Language.Css.Build", "Language.Css.Syntax"] ''StyleBody False [])
$(devArbitrary ''StyleBodyAction)
$(devArbitraryWithActions False ''StyleBody)
 
$(devActions ["Language.Css.Build", "Language.Css.Syntax"] ''StyleSheet False [])
$(devArbitrary ''StyleSheetAction)
$(devArbitraryWithActions False ''StyleSheet)

$(devArbitrary ''MCssFile)

encodeMCssFile x = LC8.pack $ (render (pretty x)) 

mencode :: MCssFile -> LC8.ByteString
mencode = encodeMCssFile

