{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module Pandoc where

import Test.QuickCheck

import Text.Pandoc.Definition
import Text.Pandoc.Writers.RTF
import Text.Pandoc.Writers.Docx
import Text.Pandoc.Writers.ODT

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC8

import Data.DeriveTH
import Data.Map
import Data.Default
import DeriveArbitrary

import Vector
import ByteString
import System.IO.Unsafe
import Data.Char (chr)

-- $(deriveArbitraryRec ''Element)

derive makeArbitrary ''Meta
derive makeArbitrary ''Format
derive makeArbitrary ''Block
derive makeArbitrary ''Inline
derive makeArbitrary ''MetaValue

instance Arbitrary (Map String MetaValue) where
   arbitrary = do
     x <- arbitrary
     y <- arbitrary 
     return $ singleton x y

$(deriveArbitraryRec ''Pandoc)
$(deriveArbitraryRec ''ListNumberStyle)
$(deriveArbitraryRec ''ListNumberDelim)
$(deriveArbitraryRec ''Alignment)
$(deriveArbitraryRec ''QuoteType)
$(deriveArbitraryRec ''MathType)
$(deriveArbitraryRec ''Citation)

mencode_rtf :: Pandoc -> LC8.ByteString
mencode_rtf x = LC8.pack $ writeRTF def x

mencode_docx :: Pandoc -> LC8.ByteString
mencode_docx x = unsafePerformIO $ writeDocx def x

mencode_odt :: Pandoc -> LC8.ByteString
mencode_odt x = unsafePerformIO $ writeODT def x
