{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.QuickFuzz.Gen.Document.PDF where

import Data.Default

import Test.QuickCheck

import qualified Data.ByteString.Lazy.Char8 as L8
import Data.DeriveTH

import Graphics.EasyRender
import Graphics.EasyRender.Internal

import Test.QuickFuzz.Derive.Actions
import Test.QuickFuzz.Derive.Arbitrary
import Test.QuickFuzz.Derive.Show
import Test.QuickFuzz.Gen.FormatInfo
import Test.QuickFuzz.Gen.Base.ByteString
import Test.QuickFuzz.Gen.Base.String

$(devActions ["Graphics.EasyRender"] ''Draw True [''()] [])
$(devArbitrary ''DrawAction)
$(devShow ''DrawAction)

instance Arbitrary (Draw ()) where
    arbitrary = performDrawAction <$> arbitrary

-- $(devActions ["Graphics.EasyRender"] ''Document True [''() ])
-- $(devArbitrary ''DocumentAction)
-- $(devArbitraryWithActions True ''Document)
-- $(devShow ''DocumentAction)

pdfInfo :: FormatInfo (Draw()) [DrawAction]
pdfInfo = def 
    { encode = L8.pack . render_string Format_PDF . newpage 1024 1024 
    , random = arbitrary
    , actions = Just $ def
        { randomActions = arbitrary
        , shrinkActions = shrinkActionList shrinkDrawAction   
        , performActions = performDrawAction }
    , ext = "pdf" 
    } 
