{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances, MultiParamTypeClasses#-}

module Lua where

import Megadeth.Prim
import DeriveArbitrary

import Test.QuickCheck
import Language.Lua.Syntax
import Language.Lua.PrettyPrinter

import Data.Text

import DeriveFixable
import Data.List
import Control.Monad.Trans
import Control.Monad.Trans.State

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC8

import Strings

instance Arbitrary String where
   arbitrary = genName

type MLUA = Block

getVId (PrefixExp (PEVar v)) = luaVar v
getVId _ = Name $ pack ""

getAId (Assign [v] e) = luaVar v
getAId _ = Name $ pack ""

luaVar (VarName n) = n
luaVar _ = Name $ pack ""

initV :: StV Name
initV = StV []

printSt (StV v) = "\nstate: " ++ (Data.List.concat $ Data.List.map (\(Name i) -> (show . unpack) i) v) ++ "\n"

popId :: Name -> VState Name ()
popId i = do st <- get
             put $ st {vars = delete i (vars st)}

pushId :: Name -> VState Name ()
pushId i = do st <- get
              put $ st {vars = i:(vars st)}

instance Fixable Name Name where
  fix = return

instance Fixable Name Text where
  fix = return

genCons :: Gen Exp
genCons = do i <- arbitrary
             return (Bool i)

genVar :: [Name] -> Gen Exp
genVar xs = do n <- elements xs
               return (PrefixExp (PEVar (VarName n)))

$(mkGranFix ''Name 'PrefixExp 'Assign ''Block)

cBlock :: MLUA -> Gen MLUA
cBlock (Block stats m) = do cstat <- evalStateT (fix stats) initV
                            return (Block cstat m)

instance {-# OVERLAPPING #-} Arbitrary Block where
  arbitrary = sized go >>= cBlock
    where go n = Block <$> (listOf $ (resize (n `div` 10) arbitrary))
                       <*> (return Nothing)--resize (max 0 (n_apyT - 1)) arbitrar

instance {-# OVERLAPPING #-} Arbitrary [(Exp, Block)] where
  arbitrary = undefined --listOf1 arbitrary

--Warning: will throw a lot of exceptions because of a lot of empty ifs
$(devArbitrary ''MLUA)

mencode :: MLUA -> LC8.ByteString
mencode = LC8.pack . show . pprint
