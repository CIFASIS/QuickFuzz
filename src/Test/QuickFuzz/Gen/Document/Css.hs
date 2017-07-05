{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Test.QuickFuzz.Gen.Document.Css where

import Data.Default

import Test.QuickCheck

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as C8

import Data.DeriveTH
import Control.DeepSeq

import Test.QuickFuzz.Derive.Actions
import Test.QuickFuzz.Derive.Arbitrary
import Test.QuickFuzz.Derive.Show
import Test.QuickFuzz.Derive.NFData

import Test.QuickFuzz.Gen.FormatInfo
import Test.QuickFuzz.Gen.Base.ByteString
import Test.QuickFuzz.Gen.Base.String

import Text.PrettyPrint

import Language.Css.Syntax
import Language.Css.Pretty
import Language.Css.Build
import Language.Css.Build.Attributes
import Language.Css.Build.Pseudos

-- $(devActions ["Text.Blaze.Html5.Attributes"] ''Attribute False [] [])

$(devActions["Language.Css.Build", "Language.Css.Build.Attributes", "Language.Css.Syntax" ] ''Attr False [''Attr] [])
$(devArbitrary ''AttrAction)
$(devArbitraryWithActions False ''Attr)

-- $(devActions ["Language.Css.Build.Attributes", "Language.Css.Syntax"] ''AttrIdent False [''AttrIdent])
-- $(devArbitrary ''AttrIdentAction)
-- $(devArbitraryWithActions False ''AttrIdent)

$(devActions ["Language.Css.Build.Pseudos", "Language.Css.Syntax"] ''PseudoVal False [] [])
$(devArbitrary ''PseudoValAction)
$(devArbitraryWithActions False ''PseudoVal)
 

$(devActions ["Language.Css.Build", "Language.Css.Syntax"] ''StyleBody False [] [])
$(devArbitrary ''StyleBodyAction)
$(devArbitraryWithActions False ''StyleBody)
 
$(devActions ["Language.Css.Build", "Language.Css.Syntax"] ''StyleSheet False [] [])
$(devArbitrary ''StyleSheetAction)
$(devArbitraryWithActions False ''StyleSheet)

$(devShow ''StyleSheetAction)

instance NFData StyleSheet where
  rnf x = ()

cssInfo :: FormatInfo StyleSheet StyleSheetAction
cssInfo = def
    { encode = L8.pack . render . pretty
    , random = arbitrary
    , actions = Just $ def 
        { randomActions = arbitrary
        , shrinkActions = shrinkStyleSheetAction
        , performActions = performStyleSheetAction }
    , ext = "css"
    } 
