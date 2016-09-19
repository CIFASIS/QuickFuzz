{-# Language TemplateHaskell, ConstraintKinds, FlexibleInstances, FlexibleContexts, IncoherentInstances, MultiParamTypeClasses #-}
{-# Language CPP #-}

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

#if MIN_VERSION_template_haskell(2,11,0)
#    define TH211MBKIND _maybe_kind
#else
#    define TH211MBKIND
#endif

-- |The state is composed of identifiers.
data StV a = StV {vars :: [a]} deriving Show

type VState a b = StateT (StV a) Gen b

-- |Fixable class
class Fixable a b where
  fix :: b -> VState a b

-- Common instances
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

-- |Extract name and number of arguments from a constructor
getStuff :: Con -> (Name, Int)
getStuff (NormalC n xs) = (n, length xs)
getStuff (RecC n xs) = (n, length xs)
getStuff _ = error "wrong constructor"

-- |Extract name from some type variable
getParName :: TyVarBndr -> Name
getParName (PlainTV n) = n
getParName (KindedTV n _) = n

-- |Takes some constructor and checks if it's either an assign or a variable. If that's the case then it makes the appropiate match;
-- if not, it creates a trivial match to fix recursively.
mkMatch :: [Name] -> [Name] -> (Name, Int) -> Q Match
mkMatch v a (n, m) = let stName = mkName "st"
                         idName = mkName "vid"
                         vp = mkName "vp"
                         getvid = mkName "getVId"
                         getName = mkName "get"
                         printStN = mkName "printSt"
                         vars = mkName "vars"
                         liftName = mkName "lift"
                         gencons = mkName "genCons"
                         genvar = mkName "genVar"
                         rName = mkName "return"
                         pushName = mkName "pushId"
                         ap = mkName "ap"
                         getaid = mkName "getAId" in
                     if elem n v then --Variable
                       do xlist <- replicateM m (newName "x")
                          let pats = map varP xlist
                          --Extract id, check if it's in the state. If it's not, replace it with one from
                          --the state, or if the state is empty, put some other expression (maybe a constant)
                          b <- [| let $(varP idName) = $(appE (varE getvid) (varE vp)) in
                                     do $(varP stName) <- $(varE getName)
                                        --traceM $ $(varE printStN) $(varE stName)  --uncomment this line for debugging
                                        case elem $(varE idName) ($(appE (varE vars) (varE stName))) of
                                            True -> return $(varE vp)
                                            False -> if null (($(appE (varE vars) (varE stName)))) then
                                                       do c <- $(varE liftName) $ $(varE gencons)
                                                          return c
                                                     else do newv <- $(varE liftName) $ ($(appE (varE genvar) (appE (varE vars) (varE stName))))
                                                             return newv |]
                          match (asP vp (conP n pats)) (normalB (returnQ b)) []
                     else if elem n a then --Assign
                       do xlist <- replicateM m (newName "x")
                          let pats = map varP xlist
                          --Extract id, check if it's in the state. If it's not, add it.
                          cpairs <- mapM mkDoB (tail xlist)
                          let (cohs, binds) = (map fst cpairs, map snd cpairs)
                          let xvars = map VarE ((head xlist):cohs)
                          let retBind = NoBindS (AppE (VarE rName) (foldl AppE (ConE n) xvars))
                          let doBodyT = return $ DoE (binds++[retBind])
                          let pushBind = NoBindS (AppE (VarE pushName) (VarE idName)) --push id to the state
                          let doBodyF = return $ DoE (binds++[pushBind,retBind])
                          b <- [| let $(varP idName) = $(appE (varE getaid) (varE ap)) in
                                   do $(varP stName) <- $(varE getName)
                                      --traceM $ $(varE printStN) $(varE stName)  --uncomment this line for debugging
                                      case elem $(varE idName) ($(appE (varE vars) (varE stName))) of
                                          True -> $(doBodyT)
                                          False -> $(doBodyF) |]
                          match (asP ap (conP n pats)) (normalB (returnQ b)) []
                     else --Other constructors
                       do xlist <- replicateM m (newName "x")
                          let pats = map varP xlist
                          --Just do a fix of the constructor arguments and return the fixed constructor
                          cpairs <- mapM mkDoB xlist
                          let (cohs, binds) = (map fst cpairs, map snd cpairs)
                          let xvars = map VarE cohs
                          let retBind = NoBindS (AppE (VarE rName) (foldl AppE (ConE n) xvars))
                          let doBody = DoE (binds++[retBind])
                          match (conP n pats) (normalB (returnQ doBody)) []

-- |Generates an expression of the form cx <- fix x
mkDoB :: Name -> Q (Name, Stmt)
mkDoB x = do cx <- newName "cx"
             let fixN = mkName "fix"
             return $ (cx, BindS (VarP cx) (AppE (VarE fixN) (VarE x)))

-- |Given a list of matches (built with mkMatch), generate a function body.
mkFixBody :: [Q Match] -> Q Exp
mkFixBody matches = let e = mkName "e" in
                      lamE [varP e] (caseE (varE e) matches)

-- |Uses Megadeth to make every Fixable instance needed
devFixLang :: Name -> [Name] -> [Name] -> Name -> Q [Dec]
devFixLang i v ka t = prevDev t (const $ return False) >>= mapM (mkFix i v ka) >>= (return . concat)

-- |Creates a Fixable instance for a type, needs information to know which constructors represent
-- the identifiers, variables and assignments
mkFix :: Name -> [Name] -> [Name] -> Name -> Q [Dec]
mkFix i v a t = do ti <- reify t
                   case ti of
                      TyConI (DataD _ _ params TH211MBKIND tcons _) -> do
                        let cstuff = map getStuff tcons
                        let names = map fst cstuff
                        let matches = map (mkMatch v a) cstuff
                        let np = length params
                        let tvars = map (varT . getParName) params
                        ii <- reify i
                        case ii of
                          TyConI (DataD _ _ ip TH211MBKIND _ _) -> do
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
                              if null ivars then [d| instance $(foldl appT (tupleT (3*nip)) ((map (appT (conT ''Arbitrary)) pvars)++(map (appT (conT ''Eq)) pvars) ++(map (appT (conT ''Show)) pvars)))
                                                            => Fixable $(conT i) $(foldl appT (conT t) pvars)  where
                                                                      fix = gg where
                                                                            gg :: $(foldl appT (conT t) pvars) -> VState $(conT i) $(foldl appT (conT t) pvars)
                                                                            gg = $(mkFixBody matches) |]
                              else [d| instance $(foldl appT (tupleT (3*nip)) ((map (appT (conT ''Arbitrary)) pvars)++(map (appT (conT ''Eq)) pvars)++(map (appT (conT ''Show)) pvars)))
                                          => Fixable $(foldl appT (conT i) pvars) $(foldl appT (conT t) pvars)  where
                                                      fix = gg where
                                                            gg :: $(foldl appT (conT t) pvars) -> VState $(foldl appT (conT i) pvars) $(foldl appT (conT t) pvars)
                                                            gg = $(mkFixBody matches) |]
                          TyConI (NewtypeD _ _ ip TH211MBKIND _ _) -> do
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
                              if null ivars then [d| instance $(foldl appT (tupleT (3*nip)) ((map (appT (conT ''Arbitrary)) pvars)++(map (appT (conT ''Eq)) pvars)++(map (appT (conT ''Show)) pvars)))
                                                            => Fixable $(conT i) $(foldl appT (conT t) pvars)  where
                                                                      fix = gg where
                                                                            gg :: $(foldl appT (conT t) pvars) -> VState $(conT i) $(foldl appT (conT t) pvars)
                                                                            gg = $(mkFixBody matches) |]
                              else [d| instance $(foldl appT (tupleT (3*nip)) ((map (appT (conT ''Arbitrary)) pvars)++(map (appT (conT ''Eq)) pvars)++(map (appT (conT ''Show)) pvars)))
                                          => Fixable $(foldl appT (conT i) pvars) $(foldl appT (conT t) pvars)  where
                                                      fix = gg where
                                                            gg :: $(foldl appT (conT t) pvars) -> VState $(foldl appT (conT i) pvars) $(foldl appT (conT t) pvars)
                                                            gg = $(mkFixBody matches) |]
                          TyConI (TySynD _ ip _) -> do
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
                              if null ivars then [d| instance $(foldl appT (tupleT (3*nip)) ((map (appT (conT ''Arbitrary)) pvars)++(map (appT (conT ''Eq)) pvars)++(map (appT (conT ''Show)) pvars)))
                                                            => Fixable $(conT i) $(foldl appT (conT t) pvars)  where
                                                                      fix = gg where
                                                                            gg :: $(foldl appT (conT t) pvars) -> VState $(conT i) $(foldl appT (conT t) pvars)
                                                                            gg = $(mkFixBody matches) |]
                              else [d| instance $(foldl appT (tupleT (3*nip)) ((map (appT (conT ''Arbitrary)) pvars)++(map (appT (conT ''Eq)) pvars)++(map (appT (conT ''Show)) pvars)))
                                          => Fixable $(foldl appT (conT i) pvars) $(foldl appT (conT t) pvars)  where
                                                      fix = gg where
                                                            gg :: $(foldl appT (conT t) pvars) -> VState $(foldl appT (conT i) pvars) $(foldl appT (conT t) pvars)
                                                            gg = $(mkFixBody matches) |]
                      --newtype case?
                      _ -> return []
