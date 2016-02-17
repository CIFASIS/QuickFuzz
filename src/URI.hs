{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module URI where

import Test.QuickCheck
--import Data.DeriveTH
import DeriveArbitrary
import Network.URI

import qualified Data.ByteString.Lazy.Char8 as LC8

$(devArbitrary ''URI)

mencode :: URI -> LC8.ByteString
mencode x = LC8.pack $ show x 
