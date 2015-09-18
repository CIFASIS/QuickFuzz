{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module Js where

import Test.QuickCheck
import Check

import Control.Monad.Zip
import Control.Exception
import Data.Binary( Binary(..), encode )

--import Text.PrettyPrint.HughesPJ
import Text.PrettyPrint.Leijen
import Language.ECMAScript3.PrettyPrint
import Language.ECMAScript3.Syntax
import Language.ECMAScript3.Syntax.Arbitrary

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.DeriveTH
import DeriveArbitrary

import Vector
import ByteString

import Data.List.Split

-- derive makeArbitrary ''JSNode
-- $(deriveArbitraryRec ''TokenPosn)
-- $(deriveArbitraryRec ''CommentAnnotation)

type MJs =  JavaScript String

--instance Arbitrary String where
--   arbitrary = oneof $ map return ["a", "b"] 

mencode :: MJs -> L.ByteString
mencode x = L8.pack $ show $ prettyPrint x

main filename cmd prop maxSuccess maxSize = let (prog, args) = (head spl, tail spl) in
    (case prop of
        "fuzz" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ fuzzprop filename prog args mencode)
        "check" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ checkprop filename prog args mencode)
        "gen" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ genprop filename prog args mencode)
    ) where spl = splitOn " " cmd
