{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts, IncoherentInstances, MultiParamTypeClasses, ConstraintKinds, ScopedTypeVariables, UndecidableInstances #-}

module Js where

import Text.PrettyPrint.Leijen
import DeriveArbitrary
import Language.ECMAScript3.PrettyPrint
import Language.ECMAScript3.Syntax
import Test.QuickCheck
--import Language.ECMAScript3.Syntax.Arbitrary

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

import DeriveFixable
import Data.List
import Control.Monad.Trans
import Control.Monad.Trans.State
import Debug.Trace

import Strings

type MJs =  JavaScript ()

instance Arbitrary String where
   arbitrary = mgenName2

--(generate $ (arbitrary :: Gen (JavaScript ()))) >>= (\x -> putStr $ (show . prettyPrint) x)

getVId (VarRef a vid) = vid

getAId (VarDecl a vid e) = vid

initV :: StV (Id a)
initV = StV []

printSt (StV v) = "\nstate: " ++ (concat $ map (\(Id _ i) -> show i) v) ++ "\n"

popId :: Eq a => Id a -> VState (Id a) ()
popId i = do st <- get
             put $ st {vars = delete i (vars st)}

pushId :: Id a -> VState (Id a) ()
pushId i = do st <- get
              put $ st {vars = i:(vars st)}

genCons :: Arbitrary a => Gen (Expression a)
genCons = resize 1 arbitrary{-do i <- arbitrary
             a <- arbitrary
             return (IntLit a i)-}

genVar :: Arbitrary a => [Id a] -> Gen (Expression a)
genVar xs = do n <- elements xs
               a <- arbitrary
               return (VarRef a n)

fStat :: Arbitrary a => Statement a -> Gen (Statement a)
fStat (VarDeclStmt a _) = do vd <- arbitrary
                             return (VarDeclStmt a [vd])
fStat x = return x

instance Arbitrary MJs where
  arbitrary = do a <- sized go
                 evalStateT (coh a) (initV :: StV (Id ()))
              where
                 go n = Script Prelude.<$> resize (max 0 (n - 1)) arbitrary
                                       <*> (listOf $ (resize (n `div` 10) arbitrary))


$(devArbitrary ''MJs)
$(mkGranCoh ''Id 'VarRef 'VarDecl ''JavaScript)

instance (Arbitrary a, Eq a, Show a) => Fixable (Id a) a where
 coh = return

tJS :: MJs
tJS = Script ()
        [VarDeclStmt () [(VarDecl () (Id () "x") (Just (VarRef () (Id () "j"))))],
         VarDeclStmt () [(VarDecl () (Id () "y") (Just (VarRef () (Id () "x"))))],
         VarDeclStmt () [(VarDecl () (Id () "z") (Just (VarRef () (Id () "p"))))],
         VarDeclStmt () [(VarDecl () (Id () "u") (Just (VarRef () (Id () "t"))))]]

mencode :: MJs -> L.ByteString
mencode x = L8.pack $ show $ prettyPrint x
