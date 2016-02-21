{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module DeriveMutation where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Test.QuickCheck
import Control.Monad
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

-- | Mutation Class
class Arbitrary a => Mutation a where
    mutt :: a -> Gen a -- ^ Given a value, mutate it.
    mut :: Gen a
    -- mut = (arbitrary :: Gen a)


howm :: Con -> (Name, Int)
howm (NormalC n xs) = (n,length xs)
howm (RecC n xs) = (n,length xs)
howm (ForallC _ _ t) = howm t
howm (InfixC _ _ _ ) = error "not yet"

as :: [Name]
as = map (\x -> mkName $ 'a':show x) ([1..] :: [Int])

-- TODO: ViewPattern, recursive?
-- + mutt (C a1 a2 .. an) = do 
--          a1' <- frequency [(10,return a1), (1,arbitrary)]
--          ...
--          return $ C a1' a2' ...
-- This is extremely expensive, isn't it?

freqE :: Name -> ExpQ
freqE var = appE (varE 'frequency) (listE [tupE [litE $ integerL 10, appE (varE 'return) (varE var)], tupE [litE $ integerL 1, varE 'arbitrary]])

muttC :: Name -> [Name] -> ExpQ
muttC c vars = doE $ map (\ x -> bindS (varP x) (freqE x)) vars 
            ++ [ noBindS $ appE (varE 'return)
                $ foldr (\x r -> appE r (varE x)) (conE c) vars]

devMutation :: Name -> Q [Dec]
devMutation name = do
    def <- reify name
    case def of -- We need constructors...
        TyConI (DataD _ _ params constructors _) -> do
            let fnm = mkName $ "mutt" -- ++ (showName name) 
            f <- funD fnm $ foldl (\ p x ->
                     let (n,cant) = howm x
                         vars = take cant as
                         vp = map varP vars
                         -- vp = foldl (\vps v -> (varP v : vps, v : ves)) ([],[]) vars
                     in
                     (clause [conP n vp] (normalB $ muttC n vars) []) 
                    : p) [] constructors
            return [f]
        -- TyConI (NewtypeD _ _ params con _) -> do
        
