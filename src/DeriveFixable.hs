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
  fix :: b -> VState a b

instance Fixable a b => Fixable a [b] where
  fix = mapM fix

instance Fixable a b => Fixable a (Maybe b) where
    fix (Nothing) = return Nothing
    fix (Just a) = do ca <- fix a
                      return $ Just ca

instance (Fixable a b, Fixable a c) => Fixable a (b,c) where
    fix (x,y) = do cx <- fix x
                   cy <- fix y
                   return (cx, cy)

instance Fixable a Char where
 fix = return

instance Fixable a Double where
 fix = return

instance Fixable a Bool where
 fix = return

instance Fixable a Integer where
 fix = return

instance Fixable a Int where
  fix = return

instance Fixable a a where
 fix = return

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
             let fixN = mkName "fix"
             return $ (cx, BindS (VarP cx) (AppE (VarE fixN) (VarE x)))  --cx <- fix x

mkFixBody matches = let e = mkName "e" in
                      lamE [varP e] (caseE (varE e) matches)

mkGranFix i v ka t = do
	prevDev t (const $ return False) >>= mapM (mkFix i v ka) >>= (return . concat)

mkFix i v a t = do ti <- reify t
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
                                                            fix = gg where
                                                                  gg :: $(conT t) -> VState $(conT i) $(conT t)
                                                                  gg = $(mkFixBody matches) |]
                              else [d| instance Fixable $(foldl appT (conT i) pvars) $(conT t) where
                                              fix = gg where
                                                    gg :: $(conT t) -> VState $(foldl appT (conT i) pvars) $(conT t)
                                                    gg = $(mkFixBody matches) |]
                            else
                              if null ivars then [d| instance $(foldl appT (tupleT (3*np)) ((map (appT (conT ''Arbitrary)) pvars)++(map (appT (conT ''Eq)) pvars) ++(map (appT (conT ''Show)) pvars)))
                                                            => Fixable $(conT i) $(foldl appT (conT t) pvars)  where
                                                                      fix = gg where
                                                                            gg :: $(foldl appT (conT t) pvars) -> VState $(conT i) $(foldl appT (conT t) pvars)
                                                                            gg = $(mkFixBody matches) |]
                              else [d| instance $(foldl appT (tupleT (3*np)) ((map (appT (conT ''Arbitrary)) pvars)++(map (appT (conT ''Eq)) pvars)++(map (appT (conT ''Show)) pvars)))
                                          => Fixable $(foldl appT (conT i) pvars) $(foldl appT (conT t) pvars)  where
                                                      fix = gg where
                                                            gg :: $(foldl appT (conT t) pvars) -> VState $(foldl appT (conT i) pvars) $(foldl appT (conT t) pvars)
                                                            gg = $(mkFixBody matches) |]
                          TyConI (NewtypeD _ _ ip _ _) -> do
                            let ivars = map (varT . getParName) ip
                            let nip = max np (length ip)
                            plist <- replicateM nip (newName "x")
                            let pvars = map varT plist
                            if null tvars then
                              if null ivars then [d| instance Fixable $(conT i) $(conT t) where
                                                            fix = gg where
                                                                  gg :: $(conT t) -> VState $(conT i) $(conT t)
                                                                  gg = $(mkFixBody matches) |]
                              else [d| instance Fixable $(foldl appT (conT i) pvars) $(conT t) where
                                              fix = gg where
                                                    gg :: $(conT t) -> VState $(foldl appT (conT i) pvars) $(conT t)
                                                    gg = $(mkFixBody matches) |]
                            else
                              if null ivars then [d| instance $(foldl appT (tupleT (3*np)) ((map (appT (conT ''Arbitrary)) pvars)++(map (appT (conT ''Eq)) pvars)++(map (appT (conT ''Show)) pvars)))
                                                            => Fixable $(conT i) $(foldl appT (conT t) pvars)  where
                                                                      fix = gg where
                                                                            gg :: $(foldl appT (conT t) pvars) -> VState $(conT i) $(foldl appT (conT t) pvars)
                                                                            gg = $(mkFixBody matches) |]
                              else [d| instance $(foldl appT (tupleT (3*np)) ((map (appT (conT ''Arbitrary)) pvars)++(map (appT (conT ''Eq)) pvars)++(map (appT (conT ''Show)) pvars)))
                                          => Fixable $(foldl appT (conT i) pvars) $(foldl appT (conT t) pvars)  where
                                                      fix = gg where
                                                            gg :: $(foldl appT (conT t) pvars) -> VState $(foldl appT (conT i) pvars) $(foldl appT (conT t) pvars)
                                                            gg = $(mkFixBody matches) |]
                      _ -> return []
