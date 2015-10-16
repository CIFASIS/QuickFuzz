{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module TTF where

import Args
import Test.QuickCheck
import DeriveArbitrary
import Check

import Control.Exception
import Data.Binary( Binary(..), encode )

import TTFInstructions

import Vector
import ByteString

import qualified Data.ByteString.Lazy as L

import Data.DeriveTH
import Data.List.Split
import Control.Monad.State

$(deriveArbitraryRec ''Op)

mkTable (str,xs) = Table str (ops xs)

encodeMTTFFont :: [(String, [Op])] -> L.ByteString
encodeMTTFFont xs = fst $ compile $ compileTables (map mkTable xs) (headTable 1 1 0 0 0 0 1 1 0 0 0 1 0)    

mencode :: [(String, [Op])] -> L.ByteString
mencode = encodeMTTFFont

main (MainArgs _ filename cmd prop maxSuccess maxSize outdir) = let (prog, args) = (Prelude.head spl, tail spl) in
    (case prop of
        "zzuf" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ zzufprop filename prog args mencode outdir)
        "radamsa" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ radamprop filename prog args mencode outdir)
        "check" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ checkprop filename prog args mencode outdir)
        "gen" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ genprop filename prog args mencode outdir)
        "exec" -> quickCheckWith stdArgs { maxSuccess = maxSuccess , maxSize = maxSize } (noShrinking $ execprop filename prog args mencode outdir)
        _     -> error "Invalid action selected"
    ) where spl = splitOn " " cmd
