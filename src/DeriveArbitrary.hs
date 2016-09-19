{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
module DeriveArbitrary (
--    module Megadeth.Prim,
--    module Megadeth.DeriveArbitrary,
    module ByteString,
    module Vector,
    module ProbGen,
    --createIntGen,
    --instaGen,
    --devIntGen
    devArbitrary,
    -- devDeriveArbitrary 
    ) where

import Megadeth.Prim
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
-- Normal Arbitrary
--
import Control.Monad
import Control.Arrow
import Control.Applicative
import Data.List

#if MIN_VERSION_template_haskell(2,11,0)
#    define TH211MBKIND _maybe_kind
#else
#    define TH211MBKIND
#endif

-- | Build the arbitrary function with makeArbs
chooseExpQ :: Name -> Name -> Name -> Integer -> Type -> ExpQ
chooseExpQ g n t bf (AppT ListT ty) = [| listOf $ resize ($(varE  n) `div` 10) arbitrary |]
chooseExpQ g n t bf ty | headOf ty /= t = [| resize (max 0 ($(varE n) - 1)) arbitrary |]
chooseExpQ g n t bf ty =
  case bf of
    0  -> [| arbitrary |] 
    1  -> [| $(varE g) $ $(varE n) - 1 |]
    bf -> [| $(varE g) $ $(varE n) `div` bf |]


makeArbs :: Name -> Name -> Name ->  [ConView] -> [ExpQ]
makeArbs g n t xs = map (fmap fixAppl) [ foldl (\h ty -> uInfixE h (varE '(<*>)) (chooseExpQ g n t bf ty)) (conE name) tys' | SimpleCon name bf tys' <- xs]

-- | Generic function used to create arbitrarily large tuples
-- @
-- do
--  a1 <- arbitrary
--  a2 <- arbitrary
--  ....
--  return $ (a1,a2,...)
-- @
genTupleArbs :: Int -> ExpQ
genTupleArbs n = 
    let ys = take n varNames
        xs = map mkName ys
         in
        doE $
             map (\x -> bindS (varP x) (varE 'arbitrary)) xs
            ++ [ noBindS $ appE (varE 'return) (tupE (map varE xs))]


-- | Give an arbitrary instance for its argument.
-- It doesn't check anything, just assume that it is ok to instance
-- its argument. And define the function arbitrary depending what type its
-- argument references to.
deriveArbitrary :: Name -> Q [Dec]
deriveArbitrary t = do
    inf <- reify t
    runIO $ print $ "Deriving:" ++ show inf
    case inf of
        TyConI (DataD _ _ params TH211MBKIND constructors _) -> do
              let ns  = map varT $ paramNames params
                  scons = map (simpleConView t) constructors
                  fcs = filter ((==0) . bf) scons
                  gos g n = -- Fancy gos
                      if length scons > 1
                      then
                            if length fcs == length scons
                            then
                                [| oneof $(listE (makeArbs g n t fcs))|]
                            else
                                if length fcs > 1 
                                then
                                    [|  if $(varE n) <= 1
                                    then oneof $(listE (makeArbs g n t fcs))
                                    else oneof $(listE (makeArbs g n t scons))|]
                                else
                                    [|  if $(varE n) <= 1
                                    then $(head (makeArbs g n t fcs))
                                    else oneof $(listE (makeArbs g n t scons))|]
                      else
                            [| $(head (makeArbs g n t scons)) |]
              if not $ null ns then
               [d| instance $(applyTo (tupleT (length ns)) (map (appT (conT ''Arbitrary)) ns))
                            => Arbitrary $(applyTo (conT t) ns) where
                              arbitrary = sized go
                                where go n = $(gos 'go 'n) |]
               else
                [d| instance Arbitrary $(applyTo (conT t) ns) where
                               arbitrary = sized go
                                 where go n = $(gos 'go 'n)|]
        TyConI (NewtypeD _ _ params TH211MBKIND con _) -> do
            let ns = map varT $ paramNames params
                scon = simpleConView t con
            if not $ null ns then
               [d| instance $(applyTo (tupleT (length ns)) (map (appT (conT ''Arbitrary)) ns))
                            => Arbitrary $(applyTo (conT t) ns) where
                              arbitrary = sized go 
                                where go n = $(head (makeArbs 'go 'n t [scon])) |]
               else
                [d| instance Arbitrary $(applyTo (conT t) ns) where
                               arbitrary = sized go
                                where go n = $(head (makeArbs 'go 'n t [scon])) |]
        TyConI inp@(TySynD _ params ty) ->
            case (getTy ty) of
                (TupleT n) -> 
                        let ns = map varT $ paramNames params in
                        if not $ null ns then
                           [d| instance $(applyTo (tupleT (length ns)) (map (appT (conT ''Arbitrary)) ns))
                                        => Arbitrary $(applyTo (conT t) ns) where
                                          arbitrary = $(genTupleArbs n) |]
                        else
                           [d| instance Arbitrary $(applyTo (conT t) ns) where
                                          arbitrary = $(genTupleArbs n) |]
                (ConT n) -> return [] -- This type should had been derived already,
                                        -- It is clearly a dependency and it
                                        -- should be put before in the topsort.
                _ -> do
                     runIO $ print "IGNORING"
                     runIO $ print ty
                     return [] 
        d -> do
          if (isPrim inf) then return [] else
            (fail $ "Case not defined: " ++ show d)

isArbInsName = isinsName ''Arbitrary

devArbitrary :: Name -> Q [Dec]
devArbitrary = megaderive deriveArbitrary isArbInsName 

-- devDeriveArbitrary :: Name -> Q [Dec]
-- devDeriveArbitrary = megaderive (derive makeArbitrary) isArbInsName  

{-
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
    let listaFreq' nmuu = listE $ reverse $ foldl
                     (\res (c,bs) ->
                        (foldl
                            (\r b -> appE (appE (varE '(<*>)) r)
                                (case b of
                                    Nothing -> sizedarb
                                    Just True -> appE (appE (varE fname) (varE lis)) nmuu
                                    _ -> appE (appE (varE fname) dropold) nmuu
                                     )) (appE (varE 'pure) (conE c)) bs) : res)
                     [] cons
    {-
       let listaFreq'' = appE (appE (varE 'zip) (varE lis)) $ listE $ reverse $ foldl
                               (\res (c,bs) ->
                               if null bs then
                                   [|return $(conE c)|] : res
                               else res) [] cons
    -}
    {-
       let listaFreq'' = listE $ reverse $ foldl
                        (\res (c,bs) ->
                           if (and $ map isJust bs)
                               then res else
                               (foldl (\r x -> [|$(r) <*> arbitrary |]) (appE (varE 'pure) (conE c)) bs) : res)
                        [] cons
    -}
    let listaFreq = appE (appE (varE 'zip) (varE lis)) (listaFreq' nmu)
    funD fname $
        [ clause [varP lis, [p|0|]] (normalB $ [|resize 0 arbitrary|]) []
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
         clause [varP lis, [p|0|]] (normalB $ [|resize 0 arbitrary|]) []
        ,clause [varP lis, varP n] (normalB $
                foldl (\res mb ->
                        infixE
                            (Just res)
                            (varE '(<*>))
                            $ Just $ case mb of
                                    Nothing -> [|resize $(varE n) arbitrary|]
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
                    AppT ListT  _ -> Just False
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
                d -> return $ Left $ "Not ready for " ++ show d
                
        TyConI (DataD _ _ params TH211MBKIND constructors _) ->
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

instaGen :: Name -> Q [Dec]
instaGen mm = [d|instance ProbGen $(conT mm) where
                    prob_gen _ n = resize n arbitrary |]

devIntGen :: Name -> Q [Dec]
devIntGen = megaderive createIntGen (const $ return False) isGenName
-}

