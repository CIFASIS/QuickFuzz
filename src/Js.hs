{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module Js where

import Args
import Test.QuickCheck
import Check

import Text.PrettyPrint.Leijen
import Language.ECMAScript3.PrettyPrint
import Language.ECMAScript3.Syntax
import Language.ECMAScript3.Syntax.Arbitrary

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

import Data.List.Split

type MJs =  JavaScript String

mencode :: MJs -> L.ByteString
mencode x = L8.pack $ show $ prettyPrint x

jsmain (MainArgs _ cmd filename prop maxSuccess maxSize outdir b) = let (prog, args) = (head spl, tail spl) in
    (case prop of
        "zzuf" -> quickCheckWith stdArgs { chatty = not b, maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ zzufprop filename prog args mencode outdir)
        "check" -> quickCheckWith stdArgs { chatty = not b, maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ checkprop filename prog args mencode outdir)
        "gen" -> quickCheckWith stdArgs { chatty = not b, maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ genprop filename prog args mencode outdir)
        "exec" -> quickCheckWith stdArgs { chatty = not b, maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ execprop filename prog args mencode outdir)
        _     -> error "Invalid action selected"
    ) where spl = splitOn " " cmd


main fargs False = jsmain $ fargs ""
main fargs True  = processPar fargs jsmain
