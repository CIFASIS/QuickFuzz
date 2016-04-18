{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module DeriveArbitrary (
    module Megadeth.Prim,
    module Megadeth.DeriveArbitrary,
    module Misc,
    module ByteString,
    module Vector
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
-- Gen

as :: [Name]
as = map (\x -> mkName $ 'a':show x) ([1..] :: [Int])

-- TODO: sized custom fun
customFun :: Name -> [(Name,[Bool])] -> Q Dec -- Can I give the type too?
customFun fname cons = 
    let lis = mkName "xs" in
    funD fname $
        [ clause [listP []] (normalB $ varE 'arbitrary) []
        , clause [listP [varP lis]]
            (normalB $
             appE   (varE 'frequency)
                    (listE
                        (snd $ (foldl (\(p,res) (c,bs) ->
                             (p+1, (tupE
                    [appE (appE (varE '(!!)) (varE lis)) (litE $ IntPrimL p)
                    ,foldl (\r b -> appE r (if b 
                                            then appE (varE fname) (varE lis)
                                            else varE 'arbitrary)) (varE c) bs
                    ]) : res)) (0,[]) cons))))
             []
        ]

customG :: Name -> Q [Dec] -- Just one function
customG n = do
    def <- reify n
    case def of
        TyConI (TySynD _ _ _) ->  return [] -- later
        TyConI (DataD _ _ _ constructors _) -> do
            let fnm = mkName $ "customGen" -- link this name with the typename
            f <- (customFun fnm (foldl (\p c -> 
                let
                    SimpleCon n rec vs = simpleConView n c
                    tfs = map (\ty -> (countCons (== n) ty) > 0 ) vs
                in (n,tfs) : p) [] constructors))
            return [f]
            

createIntGen :: Name -> Q [Dec]
createIntGen n = do
    arb <- devArbitrary n -- n should have and arbitrary instance, and doing so we get all the dependencies as well
    cstm <- customG n
    return (arb ++ cstm)
