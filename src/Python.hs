{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, IncoherentInstances, MultiParamTypeClasses, ConstraintKinds#-}

module Python where

import DeriveFixable as F
import DeriveArbitrary
import Test.QuickCheck
import Language.Python.Common
import Language.Python.Common.Pretty

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC8

import Strings
import Data.Char
import Data.Data
import Data.List
import Control.Monad
import System.IO.Unsafe
import Data.Bifunctor
import Control.Monad.Trans
import Control.Monad.Trans.State

instance Arbitrary String where
   arbitrary = genName
{-
--Possibly incomplete
isReservedWord :: String -> Bool
isReservedWord w = elem w rwords
  where rwords = keywords ++ constants
        keywords = ["and", "as", "assert", "break", "class", "continue", "def", "del", "elif", "else", "except", "exec", "finally",
                    "for", "from", "global", "if", "import", "in", "is", "lambda", "not", "or", "pass", "print", "raise", "return",
                    "try", "while", "with", "yield"]
        constants = ["True", "False", "None"]

--python reference 2.3, missing other_start
isValidStart :: Char -> Bool
isValidStart c = unicodeLetter c || c == '_'
  where unicodeLetter c = case generalCategory c of
                            UppercaseLetter -> True
                            LowercaseLetter -> True
                            TitlecaseLetter -> True
                            ModifierLetter  -> True
                            OtherLetter     -> True
                            LetterNumber    -> True
                            _               -> False

--python reference 2.3, missing other_continue
isValidContinue :: Char -> Bool
isValidContinue c = isValidStart c || validContinue c
  where validContinue c = case generalCategory c of
                            NonSpacingMark       -> True
                            SpacingCombiningMark -> True
                            DecimalNumber        -> True
                            ConnectorPunctuation -> True
                            _                    -> False

--Generation of easy to read identifiers
class ReadableIds a where
  fixUp :: a -> Gen a

instance Arbitrary a => ReadableIds (Ident a) where
  fixUp = readableFix --return . identifierFix

start :: [Char]
start = ['a'..'z'] ++ ['A'..'Z'] ++ ['_']

cont :: [Char]
cont = start ++ ['0'..'9']

readableFix :: Arbitrary a => Ident a -> Gen (Ident a)
readableFix id = do s <- elements start
                    c <- shuffle conta
                    let nid = s:(take 2 c)
                        fid = if isReservedWord nid then '_':nid else nid
                    return (id{ident_string = fid})

--use this if you want an identifier to be made of any valid characters according to reference
identifierFix :: Ident a -> Ident a
identifierFix id = let s = ident_string id
                       fixStart c = if isValidStart c then c else '_'
                       fixContinue c = if isValidContinue c then c else '_'
                   in case s of
                     ""      -> id {ident_string = "_"}
                     (st:c)  -> let fixed = (fixStart st):(map fixContinue c)
                                in if isReservedWord fixed then id {ident_string = ('_':fixed)} else id {ident_string = fixed}
-}
type MPy = Module ()

initV = StV []

instance Arbitrary MPy where
  arbitrary = do a <- sized go
                 evalStateT (fix a) (initV :: StV (Ident ()))
              where
                 go n = Module <$> (listOf $ (resize (n `div` 10) arbitrary))

instance {-# OVERLAPPING #-} Arbitrary a => Arbitrary (Ident a) where --easy to read instance
     arbitrary = sized go {->>= fixUp-} where
           go n = Ident <$> resize n arbitrary <*> resize n arbitrary

gInt :: Arbitrary a => Bool -> Gen(Expr a)
gInt b = do i <- arbitrary
            a <- arbitrary
            if b then return (Int i (show i) a)
                 else return (LongInt i (show i) a)

gFloat :: Arbitrary a => Bool -> Gen(Expr a)
gFloat b = do i <- arbitrary
              a <- arbitrary
              if b then return (Float i (show i) a)
                   else return (Imaginary i (show i) a)

instance {-# OVERLAPPING #-} Arbitrary a => Arbitrary (Expr a) where
     arbitrary = sized go where
           go n | (n <= 1) = oneof
                               [Var <$> resize n arbitrary <*> resize n arbitrary,
                                gInt True,
                                gInt False,
                                gFloat True,
                                gFloat False,
                                Bool <$> resize n arbitrary <*> resize n arbitrary,
                                None <$> resize n arbitrary,
                                Ellipsis <$> resize n arbitrary,
                                ByteStrings <$> (listOf $ (resize (n `div` 10) arbitrary))
                                            <*> resize n arbitrary,
                                Strings <$> (listOf $ (resize (n `div` 10) arbitrary))
                                        <*> resize n arbitrary,
                                UnicodeStrings <$> (listOf $ (resize (n `div` 10) arbitrary))
                                               <*> resize n arbitrary,
                                Yield <$> resize n arbitrary <*> resize n arbitrary,
                                {-Generator <$> resize n arbitrary <*> resize n arbitrary,-}
                                {-ListComp <$> resize n arbitrary <*> resize n arbitrary,-}
                                Dictionary <$> (listOf $ (resize (n `div` 10) arbitrary))
                                           <*> resize n arbitrary{-,
                                DictComp <$> resize n arbitrary <*> resize n arbitrary,
                                SetComp <$> resize n arbitrary <*> resize n arbitrary-}]
             | otherwise = frequency [(2, Var <$> resize n arbitrary <*> resize n arbitrary),
                                      (2, gInt True),
                                      (2, gInt False),
                                      (2, gFloat True),
                                      (2, gFloat False),
                                      (2, Bool <$> resize n arbitrary <*> resize n arbitrary),
                                      (2, None <$> resize n arbitrary),
                                      (2, Ellipsis <$> resize n arbitrary),
                                      (2, ByteStrings <$> (listOf $ (resize (n `div` 10) arbitrary))
                                                      <*> resize n arbitrary),
                                      (2, Strings <$> (listOf $ (resize (n `div` 10) arbitrary))
                                                  <*> resize n arbitrary),
                                      (2, UnicodeStrings <$> (listOf $ (resize (n `div` 10) arbitrary))
                                                         <*> resize n arbitrary),
                                      (1, Call <$> go (n - 1)
                                               <*> (listOf $ (resize (n `div` 10) arbitrary))
                                               <*> resize n arbitrary),
                                      (1, Subscript <$> go (n `div` 2) <*> go (n `div` 2)
                                                    <*> resize n arbitrary),
                                      (1, SlicedExpr <$> go (n - 1)
                                                     <*> (listOf $ (resize (n `div` 10) arbitrary))
                                                     <*> resize n arbitrary),
                                      (1, CondExpr <$> go (n `div` 3) <*> go (n `div` 3)
                                                   <*> go (n `div` 3) <*> resize n arbitrary),
                                      (1, BinaryOp <$> resize n arbitrary <*> go (n `div` 2)
                                                   <*> go (n `div` 2) <*> resize n arbitrary),
                                      (1, UnaryOp <$> resize n arbitrary <*> go (n - 1)
                                                  <*> resize n arbitrary),
                                      (1, Dot <$> go (n - 1) <*> resize n arbitrary
                                              <*> resize n arbitrary),
                                      (1, Lambda <$> (listOf $ (resize (n `div` 10) arbitrary))
                                                 <*> go (n - 1) <*> resize n arbitrary),
                                      (1, Tuple <$> (listOf $ (resize (n `div` 10) arbitrary))
                                                <*> resize n arbitrary),
                                      (2, Yield <$> resize n arbitrary <*> resize n arbitrary),
                                      {-(2, Generator <$> resize n arbitrary <*> resize n arbitrary),-}
                                      {-(2, ListComp <$> resize n arbitrary <*> resize n arbitrary),-}
                                      (1, List <$> (listOf $ (resize (n `div` 10) arbitrary))
                                               <*> resize n arbitrary),
                                      (2, Dictionary <$> (listOf $ (resize (n `div` 10) arbitrary))
                                                     <*> resize n arbitrary),
                                      {-(2, DictComp <$> resize n arbitrary <*> resize n arbitrary),-}
                                      (1, Set <$> (listOf $ (resize (n `div` 10) arbitrary))
                                              <*> resize n arbitrary),
                                      {-(2, SetComp <$> resize n arbitrary <*> resize n arbitrary),-}
                                      (1, Starred <$> go (n - 1) <*> resize n arbitrary),
                                      (1, Paren <$> go (n - 1) <*> resize n arbitrary),
                                      (1, StringConversion <$> go (n - 1) <*> resize n arbitrary)]

oneVar :: Arbitrary a => Gen (Expr a)
oneVar = Var <$> arbitrary <*> arbitrary

lOfOneVar :: Arbitrary a => Gen [(Expr a)]
lOfOneVar = do ov <- oneVar
               return [ov]

instance {-# OVERLAPPING #-} Arbitrary a => Arbitrary (Statement a) where
     arbitrary = sized go where
           go n | (n <= 1) = oneof [Import <$> (listOf $ (resize (n `div` 10) arbitrary))
                                           <*> resize n arbitrary,
                                    FromImport <$> resize n arbitrary <*> resize n arbitrary
                                               <*> resize n arbitrary,
                                    While <$> resize n arbitrary <*> resize n arbitrary
                                          <*> resize n arbitrary <*> resize n arbitrary,
                                    For <$> lOfOneVar --(listOf $ (resize (n `div` 10) arbitrary))
                                        <*> resize n arbitrary <*> resize n arbitrary
                                        <*> resize n arbitrary <*> resize n arbitrary,
                                    Fun <$> resize n arbitrary <*> (listOf $ (resize (n `div` 10) arbitrary))
                                        <*> resize n arbitrary <*> resize n arbitrary
                                        <*> resize n arbitrary,
                                    Class <$> resize n arbitrary
                                          <*> (listOf $ (resize (n `div` 10) arbitrary))
                                          <*> resize n arbitrary <*> resize n arbitrary,
                                    Conditional <$> (listOf $ (resize (n `div` 10) arbitrary))
                                                <*> resize n arbitrary <*> resize n arbitrary,
                                    Assign <$> lOfOneVar --(listOf $ (resize (n `div` 10) arbitrary))
                                           <*> resize n arbitrary <*> resize n arbitrary,
                                    AugmentedAssign <$> resize n arbitrary <*> resize n arbitrary
                                                    <*> resize n arbitrary <*> resize n arbitrary,
                                    Return <$> resize n arbitrary <*> resize n arbitrary,
                                    Try <$> resize n arbitrary <*> (listOf $ (resize (n `div` 10) arbitrary))
                                        <*> resize n arbitrary <*> resize n arbitrary
                                        <*> resize n arbitrary,
                                    Raise <$> resize n arbitrary <*> resize n arbitrary,
                                    With <$> (listOf $ (resize (n `div` 10) arbitrary))
                                         <*> resize n arbitrary <*> resize n arbitrary,
                                    Pass <$> resize n arbitrary,
                                    Break <$> resize n arbitrary,
                                    Continue <$> resize n arbitrary,
                                    Delete <$> (listOf $ (resize (n `div` 10) arbitrary))
                                           <*> resize n arbitrary,
                                    StmtExpr <$> resize n arbitrary <*> resize n arbitrary,
                                    Global <$> (listOf $ (resize (n `div` 10) arbitrary))
                                           <*> resize n arbitrary,
                                    NonLocal <$> (listOf $ (resize (n `div` 10) arbitrary))
                                             <*> resize n arbitrary,
                                    Assert <$> (listOf $ (resize (n `div` 10) arbitrary))
                                           <*> resize n arbitrary,
                                    Print <$> resize n arbitrary <*> (listOf $ (resize (n `div` 10) arbitrary))
                                          <*> resize n arbitrary <*> resize n arbitrary,
                                    Exec <$> resize n arbitrary <*> resize n arbitrary
                                         <*> resize n arbitrary]
             | otherwise = frequency [(2, Import <$> (listOf $ (resize (n `div` 10) arbitrary))
                                                 <*> resize n arbitrary),
                                      (2, FromImport <$> resize n arbitrary <*> resize n arbitrary
                                                     <*> resize n arbitrary),
                                      (2, While <$> resize n arbitrary <*> resize n arbitrary
                                                <*> resize n arbitrary <*> resize n arbitrary),
                                      (2, For <$> (listOf $ (resize (n `div` 10) arbitrary))
                                              <*> resize n arbitrary <*> resize n arbitrary
                                              <*> resize n arbitrary <*> resize n arbitrary),
                                      (2, Fun <$> resize n arbitrary
                                              <*> (listOf $ (resize (n `div` 10) arbitrary))
                                              <*> resize n arbitrary <*> resize n arbitrary
                                              <*> resize n arbitrary),
                                      (2, Class <$> resize n arbitrary
                                                <*> (listOf $ (resize (n `div` 10) arbitrary))
                                                <*> resize n arbitrary <*> resize n arbitrary),
                                      (2, Conditional <$> (listOf $ (resize (n `div` 10) arbitrary))
                                                      <*> resize n arbitrary <*> resize n arbitrary),
                                      (2, Assign <$> lOfOneVar --(listOf $ (resize (n `div` 10) arbitrary))
                                                 <*> resize n arbitrary <*> resize n arbitrary),
                                      (2, AugmentedAssign <$> resize n arbitrary <*> resize n arbitrary
                                                          <*> resize n arbitrary <*> resize n arbitrary),
                                      (1, Decorated <$> (listOf $ (resize (n `div` 10) arbitrary))
                                                    <*> go (n - 1) <*> resize n arbitrary),
                                      (2, Return <$> resize n arbitrary <*> resize n arbitrary),
                                      (2, Try <$> resize n arbitrary
                                              <*> (listOf $ (resize (n `div` 10) arbitrary))
                                              <*> resize n arbitrary <*> resize n arbitrary
                                              <*> resize n arbitrary),
                                      (2, Raise <$> resize n arbitrary <*> resize n arbitrary),
                                      (2, With <$> (listOf $ (resize (n `div` 10) arbitrary))
                                               <*> resize n arbitrary <*> resize n arbitrary),
                                      (2, Pass <$> resize n arbitrary),
                                      (2, Break <$> resize n arbitrary),
                                      (2, Continue <$> resize n arbitrary),
                                      (2, Delete <$> (listOf $ (resize (n `div` 10) arbitrary))
                                                 <*> resize n arbitrary),
                                      (2, StmtExpr <$> resize n arbitrary <*> resize n arbitrary),
                                      (2, Global <$> (listOf $ (resize (n `div` 10) arbitrary))
                                                 <*> resize n arbitrary),
                                      (2, NonLocal <$> (listOf $ (resize (n `div` 10) arbitrary))
                                                   <*> resize n arbitrary),
                                      (2, Assert <$> (listOf $ (resize (n `div` 10) arbitrary))
                                                 <*> resize n arbitrary),
                                      (2, Print <$> resize n arbitrary
                                                <*> (listOf $ (resize (n `div` 10) arbitrary))
                                                <*> resize n arbitrary <*> resize n arbitrary),
                                      (2, Exec <$> resize n arbitrary <*> resize n arbitrary
                                               <*> resize n arbitrary)]

$(devArbitrary ''MPy)

mencode :: MPy -> LC8.ByteString
mencode x = LC8.pack $ prettyText x

--------------------------
----Coherent variables----
--------------------------
getVId (Var i _) = i
getVId _ = Ident "" undefined

getAId (Assign [v] _ _) = getVId v
getAId (For [v] _ _ _ _) = getVId v
getAId _ = Ident "" undefined

printSt (StV v) = "\nstate: " ++ (concat $ map (\(Ident i _) -> show i) v) ++ "\n"

popId :: Eq a => Ident a -> F.VState (Ident a) ()
popId i = do st <- get
             put $ st {F.vars = delete i (F.vars st)}

pushId :: Ident a -> F.VState (Ident a) ()
pushId i = do st <- get
              put $ st {F.vars = i:(F.vars st)}

genCons :: Arbitrary a => Gen (Expr a)
genCons = do i <- arbitrary
             a <- arbitrary
             return (Int i (show i) a)

genVar :: Arbitrary a => [Ident a] -> Gen (Expr a)
genVar xs = Var <$> elements xs <*> arbitrary

instance F.Fixable (Ident a) a where
  fix = return

$(devFixLang ''Ident ['Var] ['Assign, 'For] ''Module)

instance (Arbitrary a, Eq a, Show a) => F.Fixable (Ident a) (Module a) where
  fix = gg where
    gg :: Module a -> VState (Ident a) (Module a)
    gg = \e -> case e of
               Module xs -> do cxs <- fix xs;
                               return (Module cxs)
