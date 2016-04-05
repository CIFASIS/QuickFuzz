{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns#-}
{-# LANGUAGE FlexibleInstances,UndecidableInstances#-}
module DeriveMutation where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Test.QuickCheck
import Control.Monad
--import Control.Monad.Free
--import Control.Monad.Trans.Free as FT
import Control.Arrow
import Control.Applicative
import Data.List

--import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Graph as G
import qualified Data.Map.Strict as M
import Control.Monad.Trans.State.Lazy
import qualified Control.Monad.Trans.Class as TC

import DeriveArbitrary
import Mutation

howm :: Con -> (Name, Int)
howm (NormalC n xs) = (n,length xs)
howm (RecC n xs) = (n,length xs)
howm (ForallC _ _ t) = howm t
howm (InfixC _ _ _ ) = error "not yet"

as :: [Name]
as = map (\x -> mkName $ 'a':show x) ([1..] :: [Int])

-- TODO: ViewPattern, recursive?
-- + mutt (C a1 a2 .. an) = do 
--          a1' <- frequency [(10,return a1), (1,arbitrary), (1, mutt a1)]
--          ...
--          return $ C a1' a2' ...
-- This is extremely expensive, isn't it?

freqE :: Bool -> Name -> ExpQ
freqE _ var =
        appE
            (varE 'frequency)
            (listE
                [tupE [litE $ integerL 20,
                            (appE (varE 'mutt) (varE var))]
                --, tupE [litE $ integerL 1, varE 'arbitrary]
                , tupE [litE $ integerL 1, varE 'arbitrary] -- Here we could use a custom Gen
                ])

muttC :: Name -> [(Bool,Name)] -> ExpQ
muttC c [] = 
        appE
            (varE 'frequency)
            (listE
                [
                    tupE [litE $ integerL 20,
                            (appE (varE 'return) (conE c)) ]
                , tupE [litE $ integerL 1, varE 'arbitrary]
                -- , tupE [litE $ integerL 1, varE 'mut] -- Here we could use a custom Gen
                ])
muttC c vars = doE $ map (\ (b,x) -> bindS (varP x) (freqE b x)) vars 
            ++ [ noBindS $ appE (varE 'return)
                $ foldl (\r (_,x) -> appE r (varE x)) (conE c) vars]

isMutInsName = isinsName ''Mutation

ifsymHeadOf :: Name -> Q Name
ifsymHeadOf n = do
    inf <- reify n
    case inf of
        TyConI (TySynD _ _ t) -> return $ headOf t
        _ -> return n
        

devMutationRec :: Name -> Q [Dec]
devMutationRec t = do
    deps <- prevDev t (\_ -> return False)
    nosym <- mapM ifsymHeadOf deps
    let deps' = nub $ filter (not . hasArbIns) nosym  -- Get rid of all type syn ?
    -- Just ignore typesym later... We hope that prevDev get all dependencies
    -- all right, if not, we always have Arb => Mutation
    --dps <- filterM isMutInsName deps' -- Arbitrary => Mutation :(
    ds <- mapM ((flip devMutation) Nothing) deps'
    return $ concat ds

devMutation :: Name -> Maybe Name -> Q [Dec]
devMutation name customGen = do
    def <- reify name
    case def of -- We need constructors...
        TyConI (TySynD _ _ ty) -> return [] -- devMutation (headOf ty) Nothing
        TyConI (DataD _ _ params constructors _) -> do
            let fnm = mkName $ "mutt" -- ++ (showName name) 
            let f = funD fnm $ foldl (\ p c ->
                     let 
                         SimpleCon n rec vs = simpleConView name c
                         tfs = map (\ ty -> (countCons (== name) ty > 0)) vs
                         vars = take (length vs) as
                         vp = map varP vars
                     in
                     (clause [conP n vp] (normalB $ muttC n (zip tfs vars)) []) 
                    : p) [] constructors
            let ns = map varT $ paramNames params
            if length ns > 0 then
                case customGen of
                    Nothing -> do
                        dec <- instanceD (cxt $ (map (appT (conT ''Arbitrary)) ns) ++ (map (appT (conT ''Mutation)) ns))
                                ( appT (conT ''Mutation) (applyTo (conT name) ns))
                                [f]
                        return [dec]
                    Just g -> do
                        dec <- instanceD (cxt $ (map (appT (conT ''Arbitrary)) ns) ++ (map (appT (conT ''Mutation)) ns))
                                ( appT (conT ''Mutation) (applyTo (conT name) ns))
                                [f]
                        return [dec]
            else do
                case customGen of
                    Nothing -> do
                            dec <- instanceD (cxt []) [t| Mutation $(applyTo (conT name) ns) |] [f]
                            return $ dec : []
                    Just g -> do
                            dec <- instanceD (cxt []) [t| Mutation $(applyTo (conT name) ns) |] [f]
                            return $ dec : []
        a -> return []   --return [f]
        -- TyConI (NewtypeD _ _ params con _) -> do
