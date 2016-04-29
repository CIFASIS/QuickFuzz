{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module DeriveArbitrary (
    module Megadeth.Prim,
    module Megadeth.DeriveArbitrary,
    module Misc,
    module ByteString,
    module Vector,
    createIntGen,
    devIntGen
    ) where

import Megadeth.Prim
import Megadeth.DeriveArbitrary
import Misc
import ByteString
import Vector 

import ProbGen

import Data.List.Split
import Data.Maybe
-- Gen
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Test.QuickCheck
import GHC.Exts
import GHC.Types
-- Gen
--
---

as :: [Name]
as = map (\x -> mkName $ 'a':show x) ([1..] :: [Int])

customFun :: Name -> [(Name,[Maybe Bool])] -> Q Dec -- Can I give the type too?
customFun fname cons = do
    let lis = mkName "xs" 
    let n = mkName "n"
    let newl = mkName "newl"
    let dropold = appE (appE (varE 'drop) (lift $ length cons)) (varE lis)
    let nmu = (appE (appE (varE '(-)) (varE n)) ([|1|]))
    let sizedarb = appE (appE (varE 'resize) (varE n)) $ varE 'arbitrary
    let listaFreq' = listE $ reverse $ foldl
                     (\res (c,bs) ->
                        (foldl
                            (\r b -> appE (appE (varE '(<*>)) r)
                                (case b of
                                    Nothing -> sizedarb
                                    Just True -> appE (appE (varE fname) (varE lis)) nmu
                                    _ -> appE (appE (varE fname) dropold) nmu
                                     )) (appE (varE 'pure) (conE c)) bs) : res)
                     [] cons
    let listaFreq'' = listE $ reverse $ foldl
                     (\res (c,bs) ->
                        if (and $ map isJust bs)
                            then res else
                            (foldl (\r x -> [|$(r) <*> arbitrary |]) (appE (varE 'pure) (conE c)) bs) : res)
                     [] cons
    let listaFreq = appE (appE (varE 'zip) (varE lis)) listaFreq'
    funD fname $
        [ clause [listP [],[p|1|]] (normalB $ varE 'arbitrary) []
        , clause [varP lis,[p|1|]] (
         normalB $
          appE (varE 'oneof) listaFreq''
          ) []
        , clause [varP lis,varP n]
            (normalB $
             appE   (varE 'frequency)
                    listaFreq
                    )
             []
        ]

customFunNewT :: Name -> (Name,[Maybe Bool]) -> Q Dec
customFunNewT fname (cnm, mbs) = do
    let lis = mkName "xs" 
    let n = mkName "n"
    funD fname $ [
         clause [varP lis, [p|0|]] (normalB $ varE 'arbitrary) []
        ,clause [varP lis, varP n] (normalB $
                foldl (\res mb ->
                        infixE
                            (Just res)
                            (varE '(<*>))
                            $ Just $ case mb of
                                    Nothing -> varE 'arbitrary                        
                                    Just _ -> appE (appE (varE fname) (varE lis)) (varE n) 
                      ) (appE (varE 'pure) (conE cnm)) mbs
             ) []
        ]

customTup :: Name -> Name -> Int -> Q Exp
customTup xs n mbs = 
    let reccall = appE (appE (varE 'prob_gen) (varE xs)) (varE n) 
    in
        foldl (\r _ -> 
            infixE (Just r) (varE '(<*>)) $
            Just $ reccall) (appE (varE 'pure) (conE (tupleDataName mbs))) $ replicate mbs 1

compCust :: Dec -> Q Dec
compCust (FunD n _) = 
    let nm = mkName $ "sized_" ++ (showName n) 
        s = mkName "s"
    in
    funD nm [clause [varP s] (normalB $ appE (varE ('sized)) (appE (varE n) (varE s))) []]

prefixT :: Name -> String
prefixT n = let spt = splitOn "." $ show n
            in concat $ init spt

prepareArgCons (prefname,name) = map (\ty -> case ty of
                    ConT n' -> 
                             let pren' = prefixT n'
                                in
                                if (n' == name) then
                                    Just True
                                else if (prefname == pren') then
                                    Just False else Nothing
                    _ -> Nothing)

customG :: Name -> Q (Either String Dec) -- Just one function
customG name = do
    def <- reify name
    let prefname = prefixT name
    case def of
        TyConI (TySynD _ params ty) ->  --return $ Left "Syn" 
            case (getTy ty) of
                (TupleT nu) -> 
                    let ns = map varT $ paramNames params 
                        lns = length ns
                        in
                    if lns > 0 then
                        do
                            [t] <- [d| instance $(applyTo (tupleT (length ns)) (map (appT (conT ''ProbGen)) ns))
                                         => ProbGen $(applyTo (conT name) ns) where
                                        prob_gen xs 0 = $(genTupleArbs nu)
                                        prob_gen xs n = $(customTup 'xs 'n nu)|]
                            return $ Right t 
                    else
                        do
                            [t] <- [d| instance ProbGen $(applyTo (conT name) ns) where
                                        prob_gen xs 0 = $(genTupleArbs nu)
                                        prob_gen xs n = $(customTup 'xs 'n nu) |]
                            return $ Right t 
                        
                ConT n -> return $ Left $ "Already derived?" ++ show n
                
        TyConI (DataD _ _ params constructors _) ->
            let fnm = mkName "prob_gen" -- "customGen_" ++ (map (\x -> if x == '.' then '_' else
                                                                --x) $ showName name)
                ns = map varT $ paramNames params
                f = (customFun fnm $ reverse (foldl (\p c ->  -- because foldl
                    let
                        SimpleCon n rec vs = simpleConView n c
                        tfs = prepareArgCons (prefname,name) vs
                    in (n,tfs) : p) [] constructors))
            in
            (instanceD (cxt $ (map (appT (conT ''Arbitrary)) ns) ++ (map (appT (conT ''ProbGen)) ns))
                                ( appT (conT ''ProbGen) (applyTo (conT name) ns))
                                [f])
            >>= (return . Right)
        TyConI (NewtypeD _ _ params con _) ->
            let fnm = mkName "prob_gen" 
                ns = map varT $ paramNames params
                SimpleCon n rec vs = simpleConView n con
                tfs = map (\ty -> case ty of
                                            ConT n' -> 
                                                     let pren' = prefixT n'
                                                        in
                                                        if (n' == name) then
                                                            Just True
                                                        else if (prefname == pren') then
                                                            Just False else Nothing
                                            _ -> Nothing) vs
                f = customFunNewT fnm (n,tfs)
            in
            (instanceD (cxt $ (map (appT (conT ''Arbitrary)) ns) ++ (map (appT (conT ''ProbGen)) ns))
                                ( appT (conT ''ProbGen) (applyTo (conT name) ns))
                                [f])
            >>= (return . Right)
        _ -> return $ Left "No TyConI"

createIntGen :: Name -> Q [Dec]
createIntGen n = do
    --arb <- devArbitrary n -- n should have and arbitrary instance, and doing so we get all the dependencies as well
    cstm <- customG n
    --let [FunD nm _] = cstm -- this is kinda horrible
    case cstm of
        Left s -> (runIO $ print $ "Pattern not implemented:" ++ s) >> return []
        Right d -> return [d]

isGenName = isinsName ''ProbGen

devIntGen :: Name -> Q [Dec]
devIntGen = megaderive createIntGen (const $ return False) isGenName
