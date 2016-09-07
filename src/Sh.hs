{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances, ScopedTypeVariables, MultiParamTypeClasses, ConstraintKinds #-}

module Sh where

import Test.QuickCheck
import DeriveArbitrary
import Language.Bash.Pretty
import Language.Bash.Syntax
import Text.PrettyPrint
import Language.Bash.Word

import qualified Data.ByteString.Lazy as LS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC8

import DeriveFixable
import Data.List
import Control.Monad.Trans
import Control.Monad.Trans.State
import Debug.Trace

import qualified Data.ByteString.ShellEscape as SE
import Data.Word(Word8, Word16, Word32)

import Strings

type Sh = Command

instance Arbitrary String where
   arbitrary = mgenName2

instance Arbitrary SE.Bash where
   arbitrary = do
                --l <- listOf (arbitrary :: Gen Word8)
                return $ SE.escape $ BS.pack []

instance Arbitrary Parameter where
   arbitrary = Parameter <$> arbitrary <*> (return Nothing)

getVId (ParamSubst (Bare p)) = p
getVId _ = (Parameter "" Nothing)

getAId (Assign p _ _) = p

initV :: StV (Parameter)
initV = StV []

printSt (StV v) = "\nstate: " ++ (concat $ map (\(Parameter i _) -> show i) v) ++ "\n"

popId :: Parameter -> VState Parameter ()
popId i = do st <- get
             put $ st {vars = delete i (vars st)}

pushId :: Parameter -> VState Parameter ()
pushId i = do st <- get
              put $ st {vars = i:(vars st)}

genCons :: Gen Span
genCons = resize 1 arbitrary{-do i <- arbitrary
             a <- arbitrary
             return (IntLit a i)-}

genVar :: [Parameter] -> Gen Span
genVar xs = do n <- elements xs
               return (ParamSubst (Bare n))

$(mkGranCoh ''Parameter 'ParamSubst 'Assign ''Command)

instance (Arbitrary a, Eq a, Show a) => Fixable Parameter a where
  coh = return

instance Arbitrary Sh where
      arbitrary
        = do a <- sized go
             evalStateT (coh a) initV
          where
            go n
              = Command <$> resize (max 0 (n - 1)) arbitrary
                <*> (listOf $ (resize (n `div` 10) arbitrary))

tC = Command (SimpleCommand [Assign (Parameter "x" Nothing) Equals (RValue [ParamSubst (Bare (Parameter "xx" Nothing))]),
                             Assign (Parameter "y" Nothing) Equals (RValue [ParamSubst (Bare (Parameter "yy" Nothing))]),
                             Assign (Parameter "z" Nothing) Equals (RValue [ParamSubst (Bare (Parameter "zz" Nothing))])  ] [])  []

$(devArbitrary ''Sh)

mencode :: Sh -> LS.ByteString
mencode x = LS.fromStrict $ BC8.pack $ render $ pretty x
