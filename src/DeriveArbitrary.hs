{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module DeriveArbitrary (
    module Megadeth.Prim,
    module Megadeth.DeriveArbitrary,
    module Misc,
    module ByteString,
    module Vector,
    createIntGen
    ) where

import Megadeth.Prim
import Megadeth.DeriveArbitrary
import Misc
import ByteString
import Vector 

-- Gen
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Test.QuickCheck
import GHC.Exts
import GHC.Types
-- Gen

as :: [Name]
as = map (\x -> mkName $ 'a':show x) ([1..] :: [Int])

-- TODO: sized custom fun
customFun :: Name -> [(Name,[Bool])] -> Q Dec -- Can I give the type too?
customFun fname cons = do
    let lis = mkName "xs" 
    runIO $ print cons
    funD fname $
        [ clause [listP []] (normalB $ varE 'arbitrary) []
        , clause [varP lis]
            (normalB $
             appE   (varE 'frequency)
                    (listE
                        (snd $ (foldl (\(p,res) (c,bs) ->
                             ((p :: Int)+1, (tupE
                    [appE (appE (varE '(!!)) (varE lis)) ([|p|])
                    ,foldl (\r b -> appE (appE (varE '(<*>)) r) (if b 
                                            then appE (varE fname) (varE lis)
                                            else varE 'arbitrary)) (appE (varE 'pure) (conE c)) bs
                    ]) : res)) (0,[]) cons))))
             []
        ]

customG :: Name -> Q [Dec] -- Just one function
customG name = do
    def <- reify name
    case def of
        TyConI (TySynD _ _ _) ->  return [] -- later
        TyConI (DataD _ _ _ constructors _) -> do
            let fnm = mkName $ "customGen" -- link this name with the typename
            f <- (customFun fnm (foldl (\p c -> 
                let
                    SimpleCon n rec vs = simpleConView n c
                    tfs = map (\ty -> case ty of
                                        ConT n' -> (name == n')
                                        _ -> False) vs
                in (n,tfs) : p) [] constructors))
            return [f]
            

createIntGen :: Name -> Q [Dec]
createIntGen n = do
    arb <- devArbitrary n -- n should have and arbitrary instance, and doing so we get all the dependencies as well
    cstm <- customG n
    return (arb ++ cstm)
