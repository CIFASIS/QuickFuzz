{-# Language TemplateHaskell, ConstraintKinds, FlexibleInstances, FlexibleContexts, IncoherentInstances, MultiParamTypeClasses #-}

module DeriveFixable where

import Test.QuickCheck
import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Debug.Trace
import Megadeth.Prim
import Control.Monad.Trans
import Control.Monad.Trans.State

data StV a = StV {vars :: [a]} deriving Show

type VState a b = StateT (StV a) Gen b

class Fixable a b where
  coh :: b -> VState a b

instance Fixable a b => Fixable a [b] where
  coh = mapM coh

instance Fixable a b => Fixable a (Maybe b) where
    coh (Nothing) = return Nothing
    coh (Just a) = do ca <- coh a
                      return $ Just ca

instance (Fixable a b, Fixable a c) => Fixable a (b,c) where
    coh (x,y) = do cx <- coh x
                   cy <- coh y
                   return (cx, cy)

instance Fixable a Char where
 coh = return

instance Fixable a Double where
 coh = return

instance Fixable a Bool where
 coh = return

instance Fixable a Integer where
 coh = return

instance Fixable a Int where
  coh = return

instance Fixable a a where
 coh = return

getStuff :: Con -> (Name, Int)
getStuff (NormalC n xs) = (n, length xs)
getStuff (RecC n xs) = (n, length xs)
getStuff _ = error "wrong constructor"

getParName :: TyVarBndr -> Name
getParName (PlainTV n) = n
getParName (KindedTV n _) = n

mkMatch :: Name -> Name -> (Name, Int) -> Q Match
mkMatch v a (n, m) = if n == v then --Var
                       do xlist <- replicateM m (newName "x")
                          let pats = map varP xlist
                          let stName = mkName "st"
                          let idName = mkName "vid"
                          let vp = mkName "vp"
                          b <- [| let $(varP idName) = $(appE (varE (mkName "getVId")) (varE vp)) in
                                     do $(varP stName) <- $(varE (mkName "get"))
                                        traceM $ $(varE (mkName "printSt")) $(varE stName)
                                        case elem $(varE idName) ($(appE (varE (mkName "vars")) (varE stName))) of
                                            True -> return $(varE vp)
                                            False -> if null (($(appE (varE (mkName "vars")) (varE stName)))) then
                                                       do c <- $(varE (mkName "lift")) $ $(varE (mkName "genCons"))
                                                          return c
                                                     else do newv <- $(varE (mkName "lift")) $ ($(appE (varE (mkName "genVar")) (appE (varE (mkName "vars")) (varE (mkName "st")))))
                                                             return newv |]
                          match (asP vp (conP n pats)) (normalB (returnQ b)) []
                     else if n == a then --Assign
                       do xlist <- replicateM m (newName "x")
                          let pats = map varP xlist
                          cpairs <- mapM mkDoB (tail xlist)
                          let (cohs, binds) = (map fst cpairs, map snd cpairs)
                          let xvars = map VarE ((head xlist):cohs)
                          let rName = mkName "return"
                          let retBind = NoBindS (AppE (VarE rName) (foldl AppE (ConE n)  xvars))
                          let doBodyT = return $ DoE (binds++[retBind])
                          let pushName = mkName "pushId"
                          let idName = mkName "vid"
                          let stName = mkName "st"
                          let pushBind = NoBindS (AppE (VarE pushName) (VarE idName))
                          let doBodyF = return $ DoE (binds++[pushBind,retBind])
                          let ap = mkName "ap"
                          b <- [| let $(varP idName) = $(appE (varE (mkName "getAId")) (varE ap)) in
                                   do $(varP stName) <- $(varE (mkName "get"))
                                      traceM $ $(varE (mkName "printSt")) $(varE stName)
                                      case elem $(varE idName) ($(appE (varE (mkName "vars")) (varE stName))) of
                                          True -> $(doBodyT)
                                          False -> $(doBodyF) |]
                          match (asP ap (conP n pats)) (normalB (returnQ b)) []
                     else --Other constructors
                       do xlist <- replicateM m (newName "x")
                          let pats = map varP xlist
                          cpairs <- mapM mkDoB xlist
                          let (cohs, binds) = (map fst cpairs, map snd cpairs)
                          let xvars = map VarE cohs
                          let rName = mkName "return"
                          let retBind = NoBindS (AppE (VarE rName) (foldl AppE (ConE n)  xvars))
                          let doBody = DoE (binds++[retBind])
                          match (conP n pats) (normalB (returnQ doBody)) []

mkDoB :: Name -> Q (Name, Stmt)
mkDoB x = do cx <- newName "cx"
             let cohN = mkName "coh"
             return $ (cx, BindS (VarP cx) (AppE (VarE cohN) (VarE x)))  --cx <- coh x

mkCohBody matches = let e = mkName "e" in
                      lamE [varP e] (caseE (varE e) matches)

mkGranCoh i v ka t = do
	prevDev t (const $ return False) >>= mapM (mkCoh i v ka) >>= (return . concat)

mkCoh i v a t = do ti <- reify t
                   case ti of
                      TyConI (DataD _ _ params tcons _) -> do
                        let cstuff = map getStuff tcons
                        let names = map fst cstuff
                        let matches = map (mkMatch v a) cstuff
                        let np = length params
                        let tvars = map (varT . getParName) params
                        ii <- reify i
                        case ii of
                          TyConI (DataD _ _ ip _ _) -> do
                            let ivars = map (varT . getParName) ip
                            let nip = max np (length ip)
                            plist <- replicateM nip (newName "x")
                            let pvars = map varT plist
                            if null tvars then
                              if null ivars then [d| instance Fixable $(conT i) $(conT t) where
                                                            coh = gg where
                                                                  gg :: $(conT t) -> VState $(conT i) $(conT t)
                                                                  gg = $(mkCohBody matches) |]
                              else [d| instance Fixable $(foldl appT (conT i) pvars) $(conT t) where
                                              coh = gg where
                                                    gg :: $(conT t) -> VState $(foldl appT (conT i) pvars) $(conT t)
                                                    gg = $(mkCohBody matches) |]
                            else
                              if null ivars then [d| instance $(foldl appT (tupleT (3*np)) ((map (appT (conT ''Arbitrary)) pvars)++(map (appT (conT ''Eq)) pvars) ++(map (appT (conT ''Show)) pvars)))
                                                            => Fixable $(conT i) $(foldl appT (conT t) pvars)  where
                                                                      coh = gg where
                                                                            gg :: $(foldl appT (conT t) pvars) -> VState $(conT i) $(foldl appT (conT t) pvars)
                                                                            gg = $(mkCohBody matches) |]
                              else [d| instance $(foldl appT (tupleT (3*np)) ((map (appT (conT ''Arbitrary)) pvars)++(map (appT (conT ''Eq)) pvars)++(map (appT (conT ''Show)) pvars)))
                                          => Fixable $(foldl appT (conT i) pvars) $(foldl appT (conT t) pvars)  where
                                                      coh = gg where
                                                            gg :: $(foldl appT (conT t) pvars) -> VState $(foldl appT (conT i) pvars) $(foldl appT (conT t) pvars)
                                                            gg = $(mkCohBody matches) |]
                          TyConI (NewtypeD _ _ ip _ _) -> do
                            let ivars = map (varT . getParName) ip
                            let nip = max np (length ip)
                            plist <- replicateM nip (newName "x")
                            let pvars = map varT plist
                            if null tvars then
                              if null ivars then [d| instance Fixable $(conT i) $(conT t) where
                                                            coh = gg where
                                                                  gg :: $(conT t) -> VState $(conT i) $(conT t)
                                                                  gg = $(mkCohBody matches) |]
                              else [d| instance Fixable $(foldl appT (conT i) pvars) $(conT t) where
                                              coh = gg where
                                                    gg :: $(conT t) -> VState $(foldl appT (conT i) pvars) $(conT t)
                                                    gg = $(mkCohBody matches) |]
                            else
                              if null ivars then [d| instance $(foldl appT (tupleT (3*np)) ((map (appT (conT ''Arbitrary)) pvars)++(map (appT (conT ''Eq)) pvars)++(map (appT (conT ''Show)) pvars)))
                                                            => Fixable $(conT i) $(foldl appT (conT t) pvars)  where
                                                                      coh = gg where
                                                                            gg :: $(foldl appT (conT t) pvars) -> VState $(conT i) $(foldl appT (conT t) pvars)
                                                                            gg = $(mkCohBody matches) |]
                              else [d| instance $(foldl appT (tupleT (3*np)) ((map (appT (conT ''Arbitrary)) pvars)++(map (appT (conT ''Eq)) pvars)++(map (appT (conT ''Show)) pvars)))
                                          => Fixable $(foldl appT (conT i) pvars) $(foldl appT (conT t) pvars)  where
                                                      coh = gg where
                                                            gg :: $(foldl appT (conT t) pvars) -> VState $(foldl appT (conT i) pvars) $(foldl appT (conT t) pvars)
                                                            gg = $(mkCohBody matches) |]
                      _ -> return []
