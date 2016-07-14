{-# LANGUAGE TemplateHaskell, FlexibleInstances, ScopedTypeVariables, IncoherentInstances#-}

module Python where

import Test.QuickCheck
import Language.Python.Common
import Language.Python.Common.Pretty

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC8

import DeriveArbitrary
import Strings 
import Data.Char
import Data.Data
import Data.List
import Control.Monad
import Data.Bifunctor
import Control.Monad.Trans
import Control.Monad.Trans.State

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
start :: [Char]
start = ['a'..'z'] ++ ['A'..'Z'] ++ ['_']

cont :: [Char]
cont = start ++ ['0'..'9']

randomId :: Gen String
randomId = do s <- elements start
              c <- shuffle cont
              let id = s:(take 4 c)
                  fid = if isReservedWord id then '_':id else id
              return fid

--use this if you want an identifier to be made of any valid characters according to reference
identifierFix :: String -> String
identifierFix s = let fixStart c = if isValidStart c then c else '_'
                      fixContinue c = if isValidContinue c then c else '_'
                  in case s of
                    ""      -> "_"
                    (st:c)  -> let fixed = (fixStart st):(map fixContinue c)
                               in if isReservedWord fixed then '_':fixed else fixed

type MPy = Module ()

{-$(devArbitrary ''MPy)-}

instance Arbitrary a => Arbitrary (Ident a) where --easy to read instance
      arbitrary = sized go_arvR where
            --go_arvR n_arvS = Ident <$> liftM identifierFix (resize n_arvS arbitrary) <*> resize n_arvS arbitrary  
              go_arvR n_arvS = Ident <$> randomId <*> resize n_arvS arbitrary  
instance Arbitrary a => Arbitrary (Slice a) where
      arbitrary
        = sized go_arvT where
            go_arvT n_arvU = oneof [SliceProper <$> resize n_arvU arbitrary
                                                <*> resize n_arvU arbitrary
                                                <*> resize n_arvU arbitrary
                                                <*> resize n_arvU arbitrary,
                                    SliceExpr <$> resize n_arvU arbitrary <*> resize n_arvU arbitrary,
                                    SliceEllipsis <$> resize n_arvU arbitrary]
instance Arbitrary a => Arbitrary (Op a) where
      arbitrary = sized go_arvV where
            go_arvV n_arvW = oneof [And <$> resize n_arvW arbitrary,
                                    Or <$> resize n_arvW arbitrary,
                                    Not <$> resize n_arvW arbitrary,
                                    Exponent <$> resize n_arvW arbitrary,
                                    LessThan <$> resize n_arvW arbitrary,
                                    GreaterThan <$> resize n_arvW arbitrary,
                                    Equality <$> resize n_arvW arbitrary,
                                    GreaterThanEquals <$> resize n_arvW arbitrary,
                                    LessThanEquals <$> resize n_arvW arbitrary,
                                    NotEquals <$> resize n_arvW arbitrary,
                                    NotEqualsV2 <$> resize n_arvW arbitrary,
                                    In <$> resize n_arvW arbitrary,
                                    Is <$> resize n_arvW arbitrary,
                                    IsNot <$> resize n_arvW arbitrary,
                                    NotIn <$> resize n_arvW arbitrary,
                                    BinaryOr <$> resize n_arvW arbitrary,
                                    Xor <$> resize n_arvW arbitrary,
                                    BinaryAnd <$> resize n_arvW arbitrary,
                                    ShiftLeft <$> resize n_arvW arbitrary,
                                    ShiftRight <$> resize n_arvW arbitrary,
                                    Multiply <$> resize n_arvW arbitrary,
                                    Plus <$> resize n_arvW arbitrary,
                                    Minus <$> resize n_arvW arbitrary,
                                    Divide <$> resize n_arvW arbitrary,
                                    FloorDivide <$> resize n_arvW arbitrary,
                                    Invert <$> resize n_arvW arbitrary,
                                    Modulo <$> resize n_arvW arbitrary]
instance Arbitrary a => Arbitrary (ParamTuple a) where
      arbitrary = sized go_arvX where
            go_arvX n_arvY
              | (n_arvY <= 1) = ParamTupleName <$> resize n_arvY arbitrary <*> resize n_arvY arbitrary
              | otherwise = frequency [(2, ParamTupleName <$> resize n_arvY arbitrary
                                                          <*> resize n_arvY arbitrary),
                                       (1, ParamTuple <$> (listOf $ (resize (n_arvY `div` 10) arbitrary))
                                                      <*> resize n_arvY arbitrary)]
instance Arbitrary a => Arbitrary (Parameter a) where
      arbitrary = sized go_arvZ where
            go_arvZ n_arw0 = oneof [Param <$> resize n_arw0 arbitrary <*> resize n_arw0 arbitrary
                                          <*> resize n_arw0 arbitrary <*> resize n_arw0 arbitrary,
                                    VarArgsPos <$> resize n_arw0 arbitrary <*> resize n_arw0 arbitrary
                                               <*> resize n_arw0 arbitrary,
                                    VarArgsKeyword <$> resize n_arw0 arbitrary <*> resize n_arw0 arbitrary
                                                   <*> resize n_arw0 arbitrary,
                                    EndPositional <$> resize n_arw0 arbitrary,
                                    UnPackTuple <$> resize n_arw0 arbitrary <*> resize n_arw0 arbitrary
                                                <*> resize n_arw0 arbitrary]
instance Arbitrary a => Arbitrary (YieldArg a) where
      arbitrary = sized go_arw1 where
            go_arw1 n_arw2 = oneof [YieldFrom <$> resize n_arw2 arbitrary <*> resize n_arw2 arbitrary,
                                    YieldExpr <$> resize n_arw2 arbitrary]
instance Arbitrary a => Arbitrary (DictMappingPair a) where
      arbitrary = sized go_arw3 where
            go_arw3 n_arw4 = DictMappingPair <$> resize n_arw4 arbitrary <*> resize n_arw4 arbitrary
instance Arbitrary a => Arbitrary (ComprehensionExpr a) where
      arbitrary = sized go_arw5 where
            go_arw5 n_arw6 = oneof [ComprehensionExpr <$> resize n_arw6 arbitrary,
                                    ComprehensionDict <$> resize n_arw6 arbitrary]
instance Arbitrary a => Arbitrary (CompIf a) where
      arbitrary = sized go_arw7 where
            go_arw7 n_arw8 = CompIf <$> resize n_arw8 arbitrary <*> resize n_arw8 arbitrary
                                    <*> resize n_arw8 arbitrary
instance Arbitrary a => Arbitrary (CompIter a) where
      arbitrary = sized go_arw9 where
            go_arw9 n_arwa = oneof [IterFor <$> resize n_arwa arbitrary <*> resize n_arwa arbitrary,
                                    IterIf <$> resize n_arwa arbitrary <*> resize n_arwa arbitrary]
instance Arbitrary a => Arbitrary (CompFor a) where
      arbitrary = sized go_arwb where
            go_arwb n_arwc = CompFor <$> (listOf $ (resize (n_arwc `div` 10) arbitrary))
                                     <*> resize n_arwc arbitrary <*> resize n_arwc arbitrary
                                     <*> resize n_arwc arbitrary
instance Arbitrary a => Arbitrary (Comprehension a) where
      arbitrary = sized go_arwd where
            go_arwd n_arwe = Comprehension <$> resize n_arwe arbitrary <*> resize n_arwe arbitrary
                                           <*> resize n_arwe arbitrary
instance Arbitrary a => Arbitrary (Expr a) where
      arbitrary = sized go_arwf where
            go_arwf n_arwg
              | (n_arwg <= 1)
              = oneof
                  [Var <$> resize n_arwg arbitrary <*> resize n_arwg arbitrary,
                   Int <$> resize n_arwg arbitrary <*> resize n_arwg arbitrary
                       <*> resize n_arwg arbitrary,
                   LongInt <$> resize n_arwg arbitrary <*> resize n_arwg arbitrary
                       <*> resize n_arwg arbitrary,
                   Float <$> resize n_arwg arbitrary <*> resize n_arwg arbitrary
                         <*> resize n_arwg arbitrary,
                   Imaginary <$> resize n_arwg arbitrary <*> resize n_arwg arbitrary
                             <*> resize n_arwg arbitrary,
                   Bool <$> resize n_arwg arbitrary <*> resize n_arwg arbitrary,
                   None <$> resize n_arwg arbitrary,
                   Ellipsis <$> resize n_arwg arbitrary,
                   ByteStrings <$> (listOf $ (resize (n_arwg `div` 10) arbitrary))
                               <*> resize n_arwg arbitrary,
                   Strings <$> (listOf $ (resize (n_arwg `div` 10) arbitrary))
                           <*> resize n_arwg arbitrary,
                   UnicodeStrings <$> (listOf $ (resize (n_arwg `div` 10) arbitrary))
                                  <*> resize n_arwg arbitrary,
                   Yield <$> resize n_arwg arbitrary <*> resize n_arwg arbitrary,
                   {-Generator <$> resize n_arwg arbitrary <*> resize n_arwg arbitrary,-}
                   {-ListComp <$> resize n_arwg arbitrary <*> resize n_arwg arbitrary,-}
                   Dictionary <$> (listOf $ (resize (n_arwg `div` 10) arbitrary))
                              <*> resize n_arwg arbitrary{-,
                   DictComp <$> resize n_arwg arbitrary <*> resize n_arwg arbitrary,
                   SetComp <$> resize n_arwg arbitrary <*> resize n_arwg arbitrary-}]
              | otherwise
              = frequency [(2, Var <$> resize n_arwg arbitrary <*> resize n_arwg arbitrary),
                           (2, Int <$> resize n_arwg arbitrary <*> resize n_arwg arbitrary
                                   <*> resize n_arwg arbitrary),
                           (2, LongInt <$> resize n_arwg arbitrary <*> resize n_arwg arbitrary
                                       <*> resize n_arwg arbitrary),
                           (2, Float <$> resize n_arwg arbitrary <*> resize n_arwg arbitrary
                                     <*> resize n_arwg arbitrary),
                           (2, Imaginary <$> resize n_arwg arbitrary <*> resize n_arwg arbitrary
                                         <*> resize n_arwg arbitrary),
                           (2, Bool <$> resize n_arwg arbitrary <*> resize n_arwg arbitrary),
                           (2, None <$> resize n_arwg arbitrary),
                           (2, Ellipsis <$> resize n_arwg arbitrary),
                           (2, ByteStrings <$> (listOf $ (resize (n_arwg `div` 10) arbitrary))
                                           <*> resize n_arwg arbitrary),
                           (2, Strings <$> (listOf $ (resize (n_arwg `div` 10) arbitrary))
                                       <*> resize n_arwg arbitrary),
                           (2, UnicodeStrings <$> (listOf $ (resize (n_arwg `div` 10) arbitrary))
                                              <*> resize n_arwg arbitrary),
                           (1, Call <$> go_arwf (n_arwg - 1)
                                    <*> (listOf $ (resize (n_arwg `div` 10) arbitrary))
                                    <*> resize n_arwg arbitrary),
                           (1, Subscript <$> go_arwf (n_arwg `div` 2) <*> go_arwf (n_arwg `div` 2)
                                         <*> resize n_arwg arbitrary),
                           (1, SlicedExpr <$> go_arwf (n_arwg - 1)
                                          <*> (listOf $ (resize (n_arwg `div` 10) arbitrary)) 
                                          <*> resize n_arwg arbitrary),
                           (1, CondExpr <$> go_arwf (n_arwg `div` 3) <*> go_arwf (n_arwg `div` 3) 
                                        <*> go_arwf (n_arwg `div` 3) <*> resize n_arwg arbitrary),
                           (1, BinaryOp <$> resize n_arwg arbitrary <*> go_arwf (n_arwg `div` 2)
                                        <*> go_arwf (n_arwg `div` 2) <*> resize n_arwg arbitrary),
                           (1, UnaryOp <$> resize n_arwg arbitrary <*> go_arwf (n_arwg - 1) 
                                       <*> resize n_arwg arbitrary),
                           (1, Dot <$> go_arwf (n_arwg - 1) <*> resize n_arwg arbitrary 
                                   <*> resize n_arwg arbitrary),
                           (1, Lambda <$> (listOf $ (resize (n_arwg `div` 10) arbitrary))
                                      <*> go_arwf (n_arwg - 1) <*> resize n_arwg arbitrary),
                           (1, Tuple <$> (listOf $ (resize (n_arwg `div` 10) arbitrary)) 
                                     <*> resize n_arwg arbitrary),
                           (2, Yield <$> resize n_arwg arbitrary <*> resize n_arwg arbitrary),
                           {-(2, Generator <$> resize n_arwg arbitrary <*> resize n_arwg arbitrary),-}
                           {-(2, ListComp <$> resize n_arwg arbitrary <*> resize n_arwg arbitrary),-}
                           (1, List <$> (listOf $ (resize (n_arwg `div` 10) arbitrary)) 
                                    <*> resize n_arwg arbitrary),
                           (2, Dictionary <$> (listOf $ (resize (n_arwg `div` 10) arbitrary))
                                          <*> resize n_arwg arbitrary),
                           {-(2, DictComp <$> resize n_arwg arbitrary <*> resize n_arwg arbitrary),-}
                           (1, Set <$> (listOf $ (resize (n_arwg `div` 10) arbitrary))
                                   <*> resize n_arwg arbitrary),
                           {-(2, SetComp <$> resize n_arwg arbitrary <*> resize n_arwg arbitrary),-}
                           (1, Starred <$> go_arwf (n_arwg - 1) <*> resize n_arwg arbitrary), 
                           (1, Paren <$> go_arwf (n_arwg - 1) <*> resize n_arwg arbitrary),
                           (1, StringConversion <$> go_arwf (n_arwg - 1) <*> resize n_arwg arbitrary)]
instance Arbitrary a => Arbitrary (Argument a) where
      arbitrary = sized go_arwh where
            go_arwh n_arwi = oneof [ArgExpr <$> resize n_arwi arbitrary <*> resize n_arwi arbitrary,
                                    ArgVarArgsPos <$> resize n_arwi arbitrary <*> resize n_arwi arbitrary,
                                    ArgVarArgsKeyword <$> resize n_arwi arbitrary <*> resize n_arwi arbitrary,
                                    ArgKeyword <$> resize n_arwi arbitrary <*> resize n_arwi arbitrary
                                               <*> resize n_arwi arbitrary]
instance Arbitrary a => Arbitrary (AssignOp a) where
      arbitrary = sized go_arwj where
            go_arwj n_arwk = oneof [PlusAssign <$> resize n_arwk arbitrary,
                                    MinusAssign <$> resize n_arwk arbitrary,
                                    MultAssign <$> resize n_arwk arbitrary,
                                    DivAssign <$> resize n_arwk arbitrary,
                                    ModAssign <$> resize n_arwk arbitrary,
                                    PowAssign <$> resize n_arwk arbitrary,
                                    BinAndAssign <$> resize n_arwk arbitrary,
                                    BinOrAssign <$> resize n_arwk arbitrary,
                                    BinXorAssign <$> resize n_arwk arbitrary,
                                    LeftShiftAssign <$> resize n_arwk arbitrary,
                                    RightShiftAssign <$> resize n_arwk arbitrary,
                                    FloorDivAssign <$> resize n_arwk arbitrary]
instance Arbitrary a => Arbitrary (Decorator a) where
      arbitrary = sized go_arwl where
            go_arwl n_arwm = Decorator <$> resize n_arwm arbitrary
                                       <*> (listOf $ (resize (n_arwm `div` 10) arbitrary))
                                       <*> resize n_arwm arbitrary
instance Arbitrary a => Arbitrary (ExceptClause a) where
      arbitrary = sized go_arwn where
            go_arwn n_arwo = ExceptClause <$> resize n_arwo arbitrary <*> resize n_arwo arbitrary
instance Arbitrary a => Arbitrary (FromItem a) where
      arbitrary = sized go_arwp where
            go_arwp n_arwq = FromItem <$> resize n_arwq arbitrary <*> resize n_arwq arbitrary
                                      <*> resize n_arwq arbitrary
instance Arbitrary a => Arbitrary (FromItems a) where
      arbitrary = sized go_arwr where
            go_arwr n_arws = oneof [ImportEverything <$> resize n_arws arbitrary,
                                    FromItems <$> (listOf $ (resize (n_arws `div` 10) arbitrary))
                                              <*> resize n_arws arbitrary]
instance Arbitrary a => Arbitrary (ImportItem a) where
      arbitrary = sized go_arwt where
            go_arwt n_arwu = ImportItem <$> resize n_arwu arbitrary <*> resize n_arwu arbitrary
                                        <*> resize n_arwu arbitrary
instance Arbitrary a => Arbitrary (ImportRelative a) where
      arbitrary = sized go_arwv where
            go_arwv n_arww = ImportRelative <$> resize n_arww arbitrary <*> resize n_arww arbitrary
                                            <*> resize n_arww arbitrary
instance Arbitrary a => Arbitrary (RaiseExpr a) where
      arbitrary = sized go_arwx where
            go_arwx n_arwy = oneof [RaiseV3 <$> resize n_arwy arbitrary,
                                    RaiseV2 <$> resize n_arwy arbitrary]
instance Arbitrary a => Arbitrary (Statement a) where
      arbitrary = sized go_arwz where
            go_arwz n_arwA
              | (n_arwA <= 1) = oneof [Import <$> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                                              <*> resize n_arwA arbitrary,
                                       FromImport <$> resize n_arwA arbitrary <*> resize n_arwA arbitrary
                                                  <*> resize n_arwA arbitrary,
                                       While <$> resize n_arwA arbitrary <*> resize n_arwA arbitrary
                                             <*> resize n_arwA arbitrary <*> resize n_arwA arbitrary,
                                       For <$> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                                           <*> resize n_arwA arbitrary <*> resize n_arwA arbitrary
                                           <*> resize n_arwA arbitrary <*> resize n_arwA arbitrary,
                                       Fun <$> resize n_arwA arbitrary <*> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                                           <*> resize n_arwA arbitrary <*> resize n_arwA arbitrary
                                           <*> resize n_arwA arbitrary,
                                       Class <$> resize n_arwA arbitrary
                                             <*> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                                             <*> resize n_arwA arbitrary <*> resize n_arwA arbitrary,
                                       Conditional <$> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                                                   <*> resize n_arwA arbitrary <*> resize n_arwA arbitrary,
                                       Assign <$> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                                              <*> resize n_arwA arbitrary <*> resize n_arwA arbitrary,
                                       AugmentedAssign <$> resize n_arwA arbitrary <*> resize n_arwA arbitrary
                                                       <*> resize n_arwA arbitrary <*> resize n_arwA arbitrary,
                                       Return <$> resize n_arwA arbitrary <*> resize n_arwA arbitrary,
                                       Try <$> resize n_arwA arbitrary <*> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                                           <*> resize n_arwA arbitrary <*> resize n_arwA arbitrary
                                           <*> resize n_arwA arbitrary,
                                       Raise <$> resize n_arwA arbitrary <*> resize n_arwA arbitrary,
                                       With <$> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                                            <*> resize n_arwA arbitrary <*> resize n_arwA arbitrary,
                                       Pass <$> resize n_arwA arbitrary,
                                       Break <$> resize n_arwA arbitrary,
                                       Continue <$> resize n_arwA arbitrary,
                                       Delete <$> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                                              <*> resize n_arwA arbitrary,
                                       StmtExpr <$> resize n_arwA arbitrary <*> resize n_arwA arbitrary,
                                       Global <$> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                                              <*> resize n_arwA arbitrary,
                                       NonLocal <$> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                                                <*> resize n_arwA arbitrary,
                                       Assert <$> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                                              <*> resize n_arwA arbitrary,
                                       Print <$> resize n_arwA arbitrary <*> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                                             <*> resize n_arwA arbitrary <*> resize n_arwA arbitrary,
                                       Exec <$> resize n_arwA arbitrary <*> resize n_arwA arbitrary
                                            <*> resize n_arwA arbitrary]
              | otherwise = frequency [(2, Import <$> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                                                  <*> resize n_arwA arbitrary),
                                       (2, FromImport <$> resize n_arwA arbitrary <*> resize n_arwA arbitrary
                                                      <*> resize n_arwA arbitrary),
                                       (2, While <$> resize n_arwA arbitrary <*> resize n_arwA arbitrary
                                                 <*> resize n_arwA arbitrary <*> resize n_arwA arbitrary),
                                       (2, For <$> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                                               <*> resize n_arwA arbitrary <*> resize n_arwA arbitrary
                                               <*> resize n_arwA arbitrary <*> resize n_arwA arbitrary),
                                       (2, Fun <$> resize n_arwA arbitrary
                                               <*> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                                               <*> resize n_arwA arbitrary <*> resize n_arwA arbitrary
                                               <*> resize n_arwA arbitrary),
                                       (2, Class <$> resize n_arwA arbitrary
                                                 <*> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                                                 <*> resize n_arwA arbitrary <*> resize n_arwA arbitrary),
                                       (2, Conditional <$> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                                                       <*> resize n_arwA arbitrary <*> resize n_arwA arbitrary),
                                       (2, Assign <$> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                                                  <*> resize n_arwA arbitrary <*> resize n_arwA arbitrary),
                                       (2, AugmentedAssign <$> resize n_arwA arbitrary <*> resize n_arwA arbitrary
                                                           <*> resize n_arwA arbitrary <*> resize n_arwA arbitrary),
                                       (1, Decorated <$> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                                                     <*> go_arwz (n_arwA - 1) <*> resize n_arwA arbitrary),
                                       (2, Return <$> resize n_arwA arbitrary <*> resize n_arwA arbitrary),
                                       (2, Try <$> resize n_arwA arbitrary
                                               <*> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                                               <*> resize n_arwA arbitrary <*> resize n_arwA arbitrary
                                               <*> resize n_arwA arbitrary),
                                       (2, Raise <$> resize n_arwA arbitrary <*> resize n_arwA arbitrary),
                                       (2, With <$> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                                                <*> resize n_arwA arbitrary <*> resize n_arwA arbitrary),
                                       (2, Pass <$> resize n_arwA arbitrary),
                                       (2, Break <$> resize n_arwA arbitrary),
                                       (2, Continue <$> resize n_arwA arbitrary),
                                       (2, Delete <$> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                                                  <*> resize n_arwA arbitrary),
                                       (2, StmtExpr <$> resize n_arwA arbitrary <*> resize n_arwA arbitrary),
                                       (2, Global <$> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                                                  <*> resize n_arwA arbitrary),
                                       (2, NonLocal <$> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                                                    <*> resize n_arwA arbitrary),
                                       (2, Assert <$> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                                                  <*> resize n_arwA arbitrary),
                                       (2, Print <$> resize n_arwA arbitrary
                                                 <*> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                                                 <*> resize n_arwA arbitrary <*> resize n_arwA arbitrary),
                                       (2, Exec <$> resize n_arwA arbitrary <*> resize n_arwA arbitrary
                                                <*> resize n_arwA arbitrary)]
instance Arbitrary a => Arbitrary (Handler a) where
      arbitrary = sized go_arwB where
            go_arwB n_arwC = Handler <$> resize n_arwC arbitrary <*> resize n_arwC arbitrary
                                     <*> resize n_arwC arbitrary

----------------------------------------------
----------Manually set up names-----------
----------------------------------------------

data CustomID = IdVar | IdFunc | IdParam | IdClass | Other deriving (Show)

{- REVIEW!!!!!!! also global

paramList :: (Arbitrary a) => Gen ([Parameter a])
paramList = do rl <- sublistOf params
                
-}

--Change these lists to set the names you want
vars = ["Var0", "Var1", "Var2", "Var3", "Var4", "Var5", "Var6"]
fnames = ["Function0", "Function1", "Function2", "Function 3"]
params = ["Arg0", "Arg1", "Arg2", "Arg3", "Arg4", "Arg5"]
cnames = ["Class1", "Class2", "Class3"]

custToGen :: (Arbitrary a) => CustomID -> Gen (Ident a)
custToGen IdVar = genId vars
custToGen IdFunc = genId fnames
custToGen IdParam = genId params
custToGen IdClass = genId cnames
custToGen Other = arbitrary

genId :: (Arbitrary a) => [String] -> Gen (Ident a)
genId ids = sized aux where
               aux n = Ident <$> elements ids <*> resize n arbitrary

instance Arbitrary a => Arbitrary (Module a) where
      {-arbitrary = sized go_arwD where
            go_arwD n_arwE = Module <$> (listOf $ (resize (n_arwE `div` 10) arbitrary))-}
  arbitrary = arbMod

arbMaybe :: Gen a -> Gen (Maybe a)
arbMaybe gen = frequency [(1, return Nothing), (3, liftM Just gen)]

arbSlice :: Arbitrary a => Gen (Slice a)
arbSlice = sized aux where
                  aux n = oneof [SliceProper <$> (resize n $ arbMaybe arbExpr)
                                             <*> (resize n $ arbMaybe arbExpr)
                                             <*> (resize n $ arbMaybe (arbMaybe arbExpr))
                                             <*> (resize n arbitrary),
                                 SliceExpr <$> (resize n arbExpr) <*> resize n arbitrary,
                                 SliceEllipsis <$> resize n arbitrary]
{- Revise this
arbPTuple :: Arbitrary a => Gen (ParamTuple a)
arbPTuple = sized aux where
            aux n
              | (n <= 1) = ParamTupleName <$> resize n arbitrary <*> resize n arbitrary
              | otherwise = frequency [(2, ParamTupleName <$> resize n arbitrary
                                                          <*> resize n arbitrary),
                                       (1, ParamTuple <$> (listOf $ (resize (n `div` 10) arbitrary))
                                                      <*> resize n arbitrary)]-}

-- for now just normal parameters (without default or annotation)
arbParam :: Arbitrary a => Gen (Parameter a)
arbParam = sized aux where
                  aux n = Param <$> resize n (custToGen IdParam) <*> (return Nothing)
                                <*> (return Nothing) <*> resize n arbitrary

arbYield :: Arbitrary a => Gen (YieldArg a)
arbYield = sized aux where
                  aux n = oneof [YieldFrom <$> (resize n arbExpr) <*> resize n arbitrary,
                                 YieldExpr <$> (resize n arbExpr)]

arbDMP :: Arbitrary a => Gen (DictMappingPair a)
arbDMP = sized aux where
                aux n = DictMappingPair <$> (resize n arbExpr) <*> (resize n arbExpr)

arbCompExp :: Arbitrary a => Gen (ComprehensionExpr a)
arbCompExp = sized aux where
                    aux n = oneof [ComprehensionExpr <$> (resize n arbExpr),
                                   ComprehensionDict <$> (resize n arbDMP)]

arbCompIf :: Arbitrary a => Gen (CompIf a)
arbCompIf = sized aux where
                    aux n = CompIf <$> (resize n arbExpr) <*> (resize n $ arbMaybe arbCompIter)
                                   <*> resize n arbitrary

arbCompIter :: Arbitrary a => Gen (CompIter a)
arbCompIter = sized aux where
                      aux n = oneof [IterFor <$> (resize n arbCompFor) <*> resize n arbitrary,
                                     IterIf <$> (resize n arbCompIf) <*> resize n arbitrary]

arbCompFor :: Arbitrary a => Gen (CompFor a)
arbCompFor = sized aux where
                    aux n = CompFor <$> (listOf $ (resize (n `div` 10) arbExpr))
                                    <*> (resize n arbExpr) <*> (resize n $ arbMaybe arbCompIter)
                                    <*> resize n arbitrary

arbComp :: Arbitrary a => Gen (Comprehension a)
arbComp = sized aux where
            aux n = Comprehension <$> (resize n arbCompExp) <*> (resize n arbCompFor)
                                  <*> resize n arbitrary

      
--literal now shows number, needs fix to display the correct one
arbExpr :: Arbitrary a => Gen(Expr a) 
arbExpr = sized aux where
            aux n | (n <= 1) = oneof [Var <$> (resize n $ (custToGen IdVar)) <*> resize n arbitrary,
                                      Int <$> int <*> intLit <*> resize n arbitrary,
                                      LongInt <$> int <*> intLit <*> resize n arbitrary,
                                      Float <$> float <*> flLit <*> resize n arbitrary,
                                      Imaginary <$> float <*> flLit <*> resize n arbitrary,
                                      Bool <$> resize n arbitrary <*> resize n arbitrary,
                                      None <$> resize n arbitrary,
                                      Ellipsis <$> resize n arbitrary,
                                      ByteStrings <$> (listOf $ (resize (n `div` 10) arbitrary))
                                                  <*> resize n arbitrary,
                                      Strings <$> (listOf $ (resize (n `div` 10) arbitrary))
                                              <*> resize n arbitrary,
                                      UnicodeStrings <$> (listOf $ (resize (n `div` 10) arbitrary))
                                                     <*> resize n arbitrary,
                                      Yield <$> (resize n $ arbMaybe (arbYield)) <*> resize n arbitrary,
                                      {-Generator <$> (resize n arbComp) <*> resize n arbitrary,-}
                                      {-ListComp <$> (resize n arbComp) <*> resize n arbitrary,-}
                                      Dictionary <$> (listOf $ (resize (n `div` 10) arbDMP))
                                                 <*> resize n arbitrary{-,
                                      DictComp <$> (resize n arbComp) <*> resize n arbitrary,
                                      SetComp <$> (resize n arbComp) <*> resize n arbitrary-}]
              | otherwise
              = frequency [(2, Var <$> (resize n $ (custToGen IdVar)) <*> resize n arbitrary),
                           (2, Int <$> int <*> intLit <*> resize n arbitrary),
                           (2, LongInt <$> int <*> intLit <*> resize n arbitrary),
                           (2, Float <$> float <*> flLit <*> resize n arbitrary),
                           (2, Imaginary <$> float <*> flLit <*> resize n arbitrary),
                           (2, Bool <$> resize n arbitrary <*> resize n arbitrary),
                           (2, None <$> resize n arbitrary),
                           (2, Ellipsis <$> resize n arbitrary),
                           (2, ByteStrings <$> (listOf $ (resize (n `div` 10) arbitrary))
                                           <*> resize n arbitrary),
                           (2, Strings <$> (listOf $ (resize (n `div` 10) arbitrary))
                                       <*> resize n arbitrary),
                           (2, UnicodeStrings <$> (listOf $ (resize (n `div` 10) arbitrary))
                                              <*> resize n arbitrary),
                           (1, Call <$> aux (n - 1) --force it to be a function, how?
                                    <*> (listOf $ (resize (n `div` 10) arbArg))
                                    <*> resize n arbitrary),
                           (1, Subscript <$> aux (n `div` 2) <*> aux (n `div` 2)
                                         <*> resize n arbitrary),
                           (1, SlicedExpr <$> aux (n - 1)
                                          <*> (listOf $ (resize (n `div` 10) arbSlice)) 
                                          <*> resize n arbitrary),
                           (1, CondExpr <$> aux (n `div` 3) <*> aux (n `div` 3) 
                                        <*> aux (n `div` 3) <*> resize n arbitrary),
                           (1, BinaryOp <$> resize n arbitrary <*> aux (n `div` 2)
                                        <*> aux (n `div` 2) <*> resize n arbitrary),
                           (1, UnaryOp <$> resize n arbitrary <*> aux (n - 1) 
                                       <*> resize n arbitrary),
                           (1, Dot <$> aux (n - 1) <*> resize n arbitrary --revise
                                   <*> resize n arbitrary),
                           (1, Lambda <$> (listOf $ (resize (n `div` 10) arbParam))
                                      <*> aux (n - 1) <*> resize n arbitrary),
                           (1, Tuple <$> (listOf $ (resize (n `div` 10) arbExpr)) 
                                     <*> resize n arbitrary),
                           (2, Yield <$> (resize n $ arbMaybe (arbYield)) <*> resize n arbitrary),
                           {-(2, Generator <$> (resize n arbComp) <*> resize n arbitrary),-}
                           {-ListComp <$> (resize n arbComp) <*> resize n arbitrary,-}
                           (1, List <$> (listOf $ (resize (n `div` 10) arbExpr)) 
                                    <*> resize n arbitrary),
                           (2, Dictionary <$> (listOf $ (resize (n `div` 10) arbDMP))
                                                 <*> resize n arbitrary){-,
                           DictComp <$> (resize n arbComp) <*> resize n arbitrary-},
                           (1, Set <$> (listOf $ (resize (n `div` 10) arbExpr))
                                   <*> resize n arbitrary),
                           {-(2, SetComp <$> (resize n arbComp) <*> resize n arbitrary),-}
                           (1, Starred <$> aux (n - 1) <*> resize n arbitrary), 
                           (1, Paren <$> aux (n - 1) <*> resize n arbitrary),
                           (1, StringConversion <$> aux (n - 1) <*> resize n arbitrary)]
                where  
                  int = (resize n (arbitrary :: Gen Integer)) 
                  intLit = do i <- int
                              return (show i)   
                  float = (resize n (arbitrary :: Gen Double)) 
                  flLit = do f <- float
                             return (show f) 


arbArg :: Arbitrary a => Gen (Argument a)
arbArg = sized aux where
                aux n = oneof [ArgExpr <$> (resize n arbExpr) <*> resize n arbitrary,
                               ArgVarArgsPos <$> (resize n arbExpr) <*> resize n arbitrary,
                               ArgVarArgsKeyword <$> (resize n arbExpr) <*> resize n arbitrary,
                               ArgKeyword <$> resize n arbitrary <*> (resize n arbExpr)
                                          <*> resize n arbitrary]

arbDecor :: Arbitrary a => Gen (Decorator a)
arbDecor = sized aux where
                  aux n = Decorator <$> resize n arbitrary
                                    <*> (listOf $ (resize (n `div` 10) arbArg))
                                    <*> resize n arbitrary

arbExpCl :: Arbitrary a => Gen (ExceptClause a)
arbExpCl = sized aux where
                  aux n = ExceptClause <$> (resize n $ arbMaybe (liftM2 (,) arbExpr (arbMaybe arbExpr)))
                                       <*> resize n arbitrary

--same as arbitrary but here in case of future changes
arbFrIt :: Arbitrary a => Gen (FromItem a) 
arbFrIt = sized aux where
                  aux n = FromItem <$> resize n arbitrary <*> resize n arbitrary
                                   <*> resize n arbitrary

arbFrIts :: Arbitrary a => Gen (FromItems a) 
arbFrIts = sized aux where
                  aux n = oneof [ImportEverything <$> resize n arbitrary,
                                 FromItems <$> (listOf $ (resize (n `div` 10) arbFrIt))
                                           <*> resize n arbitrary]
--idem FromItem
arbImpIt :: Arbitrary a => Gen (ImportItem a) 
arbImpIt = sized aux where
                  aux n = ImportItem <$> resize n arbitrary <*> resize n arbitrary
                                     <*> resize n arbitrary

--idem
arbImpRel :: Arbitrary a => Gen (ImportRelative a) 
arbImpRel = sized aux where
                    aux n = ImportRelative <$> resize n arbitrary <*> resize n arbitrary
                                           <*> resize n arbitrary

arbRaise :: Arbitrary a => Gen (RaiseExpr a)
arbRaise= sized aux where
                  aux n = oneof [RaiseV3 <$> (resize n $ arbMaybe (liftM2 (,) arbExpr (arbMaybe arbExpr))),
                                 RaiseV2 <$> (resize n $ arbMaybe (liftM2 (,) arbExpr (arbMaybe (liftM2 (,) arbExpr (arbMaybe arbExpr)))))]

oneVar :: Arbitrary a => Gen (Expr a)
oneVar = sized aux where
                 aux n = Var <$> (resize n $ (custToGen IdVar)) <*> resize n arbitrary

lOfOneVar :: Arbitrary a => Gen [(Expr a)]
lOfOneVar = do ov <- oneVar
               return [ov]

arbStmt :: Arbitrary a => Gen (Statement a)
arbStmt = sized aux where
                 aux n | (n <= 1) = oneof [Import <$> (listOf $ (resize (n `div` 10) arbImpIt))
                                                  <*> resize n arbitrary,
                                           FromImport <$> (resize n arbImpRel) <*> (resize n arbFrIts)
                                                      <*> resize n arbitrary,
                                           While <$> (resize n arbExpr) <*> (listOf (resize (n `div` 10) arbStmt))
                                                 <*> (listOf (resize (n `div` 10) arbStmt)) <*> resize n arbitrary,
                                           For <$> lOfOneVar--(listOf $ (resize (n `div` 10) arbExpr))
                                               <*> (resize n arbExpr) <*> (listOf (resize (n `div` 10) arbStmt))
                                               <*> (listOf (resize (n `div` 10) arbStmt)) <*> resize n arbitrary,
                                           Fun <$> resize n (custToGen IdFunc) <*> (listOf $ (resize (n `div` 10) arbParam))
                                               <*> (resize n $ arbMaybe arbExpr) <*> (listOf (resize (n `div` 10) arbStmt))
                                               <*> resize n arbitrary,
                                           Class <$> resize n (custToGen IdClass)
                                                 <*> (listOf $ (resize (n `div` 10) arbArg))
                                                 <*> (listOf (resize (n `div` 10) arbStmt)) <*> resize n arbitrary,
                                           Conditional <$> (listOf $ (resize (n `div` 10) $ liftM2 (,) arbExpr (listOf (resize (n `div` 10) arbStmt))))
                                                       <*> resize n arbitrary <*> resize n arbitrary,
                                           Assign <$> lOfOneVar--(listOf $ (resize (n `div` 10) arbExpr))
                                                  <*> (resize n arbExpr) <*> resize n arbitrary,
                                           AugmentedAssign <$> oneVar --(resize n arbExpr)
                                                           <*> resize n arbitrary <*> (resize n arbExpr) <*> resize n arbitrary,
                                           Return <$> (resize n $ arbMaybe arbExpr) <*> resize n arbitrary,
                                           Try <$> (listOf (resize (n `div` 10) arbStmt)) <*> (listOf $ (resize (n `div` 10) arbHand))
                                               <*> (listOf (resize (n `div` 10) arbStmt)) <*> (listOf (resize (n `div` 10) arbStmt))
                                               <*> resize n arbitrary,
                                           Raise <$> (resize n arbRaise) <*> resize n arbitrary,
                                           With <$> (listOf $ (resize (n `div` 10) $ liftM2 (,) arbExpr (arbMaybe arbExpr)))
                                                <*> (listOf (resize (n `div` 10) arbStmt)) <*> resize n arbitrary,
                                           Pass <$> resize n arbitrary,
                                           Break <$> resize n arbitrary,
                                           Continue <$> resize n arbitrary,
                                           Delete <$> lOfOneVar --(listOf $ (resize (n `div` 10) arbExpr))
                                                  <*> resize n arbitrary,
                                           StmtExpr <$> (resize n arbExpr) <*> resize n arbitrary,
                                           Global <$> (listOf $ (resize (n `div` 10)(custToGen IdVar))) -- global names?
                                                  <*> resize n arbitrary,
                                           NonLocal <$> (listOf $ (resize (n `div` 10) (custToGen IdVar))) -- non local names?
                                                    <*> resize n arbitrary,
                                           Assert <$> (listOf $ (resize (n `div` 10) arbExpr))
                                                  <*> resize n arbitrary,
                                           Print <$> resize n arbitrary <*> (listOf $ (resize (n `div` 10) arbExpr))
                                                 <*> resize n arbitrary <*> resize n arbitrary,
                                           Exec <$> (resize n arbExpr) <*> (resize n $ arbMaybe (liftM2 (,) arbExpr (arbMaybe arbExpr)))
                                                <*> resize n arbitrary]
                      | otherwise = frequency [(2, Import <$> (listOf $ (resize (n `div` 10) arbImpIt))
                                                          <*> resize n arbitrary),
                                               (2, FromImport <$> (resize n arbImpRel) <*> (resize n arbFrIts)
                                                              <*> resize n arbitrary),
                                               (2, While <$> (resize n arbExpr) <*> (listOf (resize (n `div` 10) arbStmt))
                                                         <*> (listOf (resize (n `div` 10) arbStmt)) <*> resize n arbitrary),
                                               (2, For <$> lOfOneVar --(listOf $ (resize (n `div` 10) arbExpr))
                                                       <*> (resize n arbExpr) <*> (listOf (resize (n `div` 10) arbStmt))
                                                       <*> (listOf (resize (n `div` 10) arbStmt)) <*> resize n arbitrary),
                                               (2, Fun <$> resize n (custToGen IdFunc) <*> (listOf $ (resize (n `div` 10) arbParam))
                                                       <*> (resize n $ arbMaybe arbExpr) <*> (listOf (resize (n `div` 10) arbStmt))
                                                       <*> resize n arbitrary),
                                               (2, Class <$> resize n (custToGen IdClass) --class names?
                                                         <*> (listOf $ (resize (n `div` 10) arbArg))
                                                         <*> (listOf (resize (n `div` 10) arbStmt)) <*> resize n arbitrary),
                                               (2, Conditional <$> (listOf $ (resize (n `div` 10) $ liftM2 (,) arbExpr (listOf (resize (n `div` 10) arbStmt))))
                                                               <*> resize n arbitrary <*> resize n arbitrary),
                                               (2, Assign <$> lOfOneVar--(listOf $ (resize (n `div` 10) arbExpr))
                                                          <*> (resize n arbExpr) <*> resize n arbitrary),
                                               (2, AugmentedAssign <$> oneVar--(resize n arbExpr)
                                                                   <*> resize n arbitrary <*> (resize n arbExpr) <*> resize n arbitrary),
                                               (1, Decorated <$> (listOf $ (resize (n `div` 10) arbDecor))
                                                             <*> aux (n - 1) <*> resize n arbitrary),
                                               (2, Return <$> (resize n $ arbMaybe arbExpr) <*> resize n arbitrary),
                                               (2, Try <$> (listOf (resize (n `div` 10) arbStmt)) <*> (listOf $ (resize (n `div` 10) arbHand))
                                                       <*> (listOf (resize (n `div` 10) arbStmt)) <*> (listOf (resize (n `div` 10) arbStmt))
                                                       <*> resize n arbitrary),
                                               (2, Raise <$> (resize n arbRaise) <*> resize n arbitrary),
                                               (2, With <$> (listOf $ (resize (n `div` 10) $ liftM2 (,) arbExpr (arbMaybe arbExpr)))
                                                        <*> (listOf (resize (n `div` 10) arbStmt)) <*> resize n arbitrary),
                                               (2, Pass <$> resize n arbitrary),
                                               (2, Break <$> resize n arbitrary),
                                               (2, Continue <$> resize n arbitrary),
                                               (2, Delete <$> lOfOneVar --(listOf $ (resize (n `div` 10) arbExpr))
                                                          <*> resize n arbitrary),
                                               (2, StmtExpr <$> (resize n arbExpr) <*> resize n arbitrary),
                                               (2, Global <$> (listOf $ (resize (n `div` 10) (custToGen IdVar))) -- global names?
                                                          <*> resize n arbitrary),
                                               (2, NonLocal <$> (listOf $ (resize (n `div` 10) (custToGen IdVar))) -- non local names?
                                                            <*> resize n arbitrary),
                                               (2, Assert <$> (listOf $ (resize (n `div` 10) arbExpr))
                                                          <*> resize n arbitrary),
                                               (2, Print <$> resize n arbitrary <*> (listOf $ (resize (n `div` 10) arbExpr))
                                                         <*> resize n arbitrary <*> resize n arbitrary),
                                               (2, Exec <$> (resize n arbExpr) <*> (resize n $ arbMaybe (liftM2 (,) arbExpr (arbMaybe arbExpr)))
                                                        <*> resize n arbitrary)]
arbHand :: Arbitrary a => Gen (Handler a)
arbHand = sized aux where
                  aux n = Handler <$> (resize n arbExpCl) <*> (listOf (resize (n `div` 10) arbStmt))
                                  <*> resize n arbitrary

arbMod :: Arbitrary a => Gen (Module a)
arbMod = sized aux where
                aux n = Module <$> (listOf (resize (n `div` 10) arbStmt))

mencode :: MPy -> LC8.ByteString
mencode x = LC8.pack $ prettyText x

--Variables coherentes

data StV a = StV { gvars :: [Ident a],
                   lvars :: [[Ident a]]} deriving Show

initV :: StV a
initV = StV [] [[]]

-- >>= :: VState a b -> (b -> VState a c) -> VState a c
type VState a b = StateT (StV a) Gen b

--Saco el "scope" mas interno
popLvars :: VState a ()
popLvars = do st <- get
              put $ st {lvars = tail $ lvars st}  

--Agrego nuevo scope
pushLvars :: [Ident a] -> VState a ()
pushLvars vs = do st <- get
                  put $ st {lvars = (vs ++ (head $ lvars st)):(lvars st)}

--Agrego variables al scope actual
pushScope :: [Ident a] -> VState a ()
pushScope vs = do st <- get
                  put $ st {lvars = (vs ++ (head $ lvars st)):(tail $ lvars st)}

--Sacar variables del scope local
popScope :: Eq a => Ident a -> VState a ()
popScope i = do st <- get
                put $ st {lvars = delete i (head $ lvars st):(lvars st)}

--Para Del
delGvars :: Eq a => Ident a -> VState a ()
delGvars i = do st <- get
                put $ st {gvars = delete i (gvars st)} 

pushGvars :: [Ident a] -> VState a ()
pushGvars i = do st <- get
                 put $ st {gvars = i++(gvars st)}

createVar :: Arbitrary a => [Ident a] -> Gen (Expr a)
createVar xs = do Var <$> elements xs <*> arbitrary                  

--Modulo
cModule :: (Arbitrary a, Eq a) => Module a -> Gen (Module a)
cModule (Module stmts) = do cstmts <- evalStateT (lStmt stmts) initV
                            return (Module cstmts)

--Statements
lStmt :: (Arbitrary a, Eq a) => [Statement a] -> VState a [Statement a]
lStmt = mapM cStmt

auxM :: Monad m => (m a, m b) -> m (a, b)
auxM (m1,m2) = do a <- m1
                  b <- m2
                  return (a,b) 

cStmt :: (Arbitrary a, Eq a) => Statement a -> VState a (Statement a)
cStmt i@(Import items a) = return i
cStmt fi@(FromImport fmod fitems a) = return fi
cStmt (While cond body welse a) = do ccond <- cExpr cond
                                     cbody <- lStmt body --que pasa si el body la borra pero el else la necesita?
                                     cwelse <- lStmt welse
                                     return (While ccond cbody cwelse a)
cStmt (For [targ] gen body felse a) = let targ_id = var_ident targ in
                                          do st <- get
                                             case elem targ_id (gvars st) of --no puede ser global, hay que generar otra
                                                True -> let vars = head $ lvars st in
                                                          do new_targ <- lift $ createVar vars
                                                             pushScope [var_ident new_targ]
                                                             cgen <- cExpr gen
                                                             cbody <- lStmt body
                                                             cfelse <- lStmt felse
                                                             return (For [new_targ] gen body felse a)
                                                False -> case elem targ_id (head $ lvars st) of --si esta en las locales, bien
                                                            True -> do cgen <- cExpr gen
                                                                       cbody <- lStmt body --lo mismo que en while, que pasa?
                                                                       cfelse <- lStmt felse
                                                                       return (For [(Var targ_id a)] cgen cbody cfelse a)
                                                            False -> do pushScope [targ_id] --la agrego al scope actual
                                                                        cgen <- cExpr gen 
                                                                        cbody <- lStmt body --que pasa?
                                                                        cfelse <- lStmt felse
                                                                        return (For [(Var targ_id a)] cgen cbody cfelse a)
cStmt (Fun fname args annot body a) = let args_ids = map param_name args in
                                          do pushLvars args_ids  --los argumentos van a las variables locales
                                             cbody <- lStmt body
                                             popLvars --saco los argumentos al salir de la funcion
                                             return (Fun fname args annot cbody a)
cStmt (Class cname args body a) = do st <- get
                                     cbody <- lStmt body --guardar el estado actual?
                                     put st
                                     return (Class cname args cbody a)
{-cStmt (Conditional guards celse a) = do cguards <- sequence $ auxM $ map (bimap cExpr lStmt) guards --que pasa en el medio?
                                        ccelse <- lStmt celse    -- [(Expr, [Stmt])] --map bimap--> [(VState Expr, VState [Stmt])] --auxM--> [VState (Expr, [Stmt])] --sequence--> VState [(Expr, [Stmt])]
                                        return (Conditional cguards ccelse a) PREGUNTAR EL TIPO-}
cStmt (Assign [to] ev a) = let to_id = var_ident to in
                           do st <- get
                              case elem to_id (gvars st) of
                                  True -> do cev <- cExpr ev
                                             return (Assign [to] cev a)
                                  False -> case elem to_id (head $ lvars st) of
                                              True -> do cev <- cExpr ev
                                                         return (Assign [to] cev a)
                                              False -> do pushScope [to_id] --la agregamos al scope local
                                                          cev <- cExpr ev
                                                          return (Assign [to] cev a)
cStmt (AugmentedAssign to op ev a) = let to_id = var_ident to in
                                       do st <- get
                                          case elem to_id (gvars st) of
                                            True -> do cev <- cExpr ev
                                                       return (AugmentedAssign to op ev a)
                                            False -> case elem to_id (head $ lvars st) of
                                                        True -> do cev <- cExpr ev
                                                                   return (AugmentedAssign to op cev a)
                                                        False -> let pool = (gvars st) ++ (head $ lvars st) in --no esta, hay que sacarla de las que tenemos
                                                                     do v <- lift $ createVar pool
                                                                        cev <- cExpr ev 
                                                                        return (AugmentedAssign v op cev a)
cStmt (Decorated decs fun a) = do st <- get
                                  cfun <- cStmt fun --ignora decorators
                                  put st
                                  return (Decorated decs cfun a)
cStmt r@(Return ret a) = case ret of
                             Nothing -> return r
                             Just e -> do st <- get
                                          ce <- cExpr e
                                          put st
                                          return (Return (Just ce) a) --idem funcion
cStmt (Try body exp telse fin a) = do cbody <- lStmt body
                                      cexp <- lHand exp
                                      ctelse <- lStmt telse
                                      cfin <- lStmt fin
                                      return (Try cbody cexp ctelse cfin a) --idem while
cStmt r@(Raise expr a) = return r
cStmt (With cont body a) = do cbody <- lStmt body --ver cont
                              return (With cont cbody a)
cStmt d@(Delete [expr] a) = let expr_id = var_ident expr in --asumiendo expr = Var
                             do st <- get
                                case elem expr_id (gvars st) of
                                  True -> delGvars expr_id >> return d
                                  False -> case elem expr_id (head $ lvars st) of
                                              True -> popScope expr_id >> return d
                                              False -> return (Pass a) --si no existe, cambiar del por pass
cStmt (StmtExpr expr a) = do cexpr <- cExpr expr
                             return (StmtExpr cexpr a)
cStmt g@(Global idlist a) = pushGvars idlist >> return g
cStmt n@(NonLocal idlist a) = return n--ver bien
cStmt (Assert exprl a) = do st <- get
                            cexpr <- lExpr exprl
                            put st
                            return (Assert cexpr a)
cStmt (Print c exprl t a) = do st <- get
                               cexpr <- lExpr exprl
                               put st                          
                               return (Print c cexpr t a)
cStmt (Exec expr env a) = do st <- get
                             cexpr <- cExpr expr --entornos??
                             put st
                             return (Exec cexpr env a) --idem funcion
cStmt s = return s

--Expresiones
lExpr :: (Arbitrary a, Eq a) => [Expr a] -> VState a [Expr a]
lExpr = mapM cExpr

cExpr :: (Arbitrary a, Eq a) => Expr a -> VState a (Expr a)
cExpr v@(Var id a) = do st <- get
                        case elem id (gvars st) of
                           True -> return v
                           False -> case elem id (head $ lvars st) of
                                        True -> return v
                                        False -> let pool = (gvars st) ++ (head $ lvars st) in --no esta, hay que sacarla de las que tenemos
                                                   do newv <- lift $ createVar pool
                                                      return newv
cExpr c@(Call fun args a) = return c --ver
cExpr (Subscript subs expr a) = do csubs <- cExpr subs --revisar
                                   cexpr <- cExpr expr
                                   return (Subscript csubs cexpr a)
cExpr (SlicedExpr sle sls a) = do csle <- cExpr sle --revisar
                                  return (SlicedExpr csle sls a)
cExpr (CondExpr true cond false a) = do ccond <- cExpr cond --no se puede asignar pero puede ser que haya que cambiar algo
                                        ctrue <- cExpr true
                                        cfalse <- cExpr false
                                        return (CondExpr ccond ctrue cfalse a)
cExpr (BinaryOp op e1 e2 a) = do ce1 <- cExpr e1
                                 ce2 <- cExpr e2
                                 return (BinaryOp op ce1 ce2 a) --no deberia haber cambiado nada
cExpr (UnaryOp op e a) = do ce <- cExpr e
                            return (UnaryOp op ce a)
cExpr (Lambda args body a) = let ids = map param_name args in 
                               do pushLvars ids
                                  cbody  <- cExpr body
                                  popLvars
                                  return (Lambda args cbody a)
cExpr (Tuple exprs a) = do cexprs <- lExpr exprs
                           return (Tuple cexprs a)
cExpr (List exprs a) = do cexprs <- lExpr exprs
                          return (List cexprs a)
cExpr (Set exprs a) = do cexprs <- lExpr exprs
                         return (Set cexprs a)
cExpr (Starred expr a) = do cexpr <- cExpr expr
                            return (Starred cexpr a)
cExpr (Paren expr a) = do cexpr <- cExpr expr
                          return (Paren cexpr a)
cExpr (StringConversion expr a) = do cexpr <- cExpr expr
                                     return (StringConversion cexpr a)
cExpr x = return x


--Handler
lHand :: (Arbitrary a, Eq a) => [Handler a] -> VState a [Handler a]
lHand = mapM cHand

cHand :: (Arbitrary a, Eq a) => Handler a -> VState a (Handler a)
cHand (Handler cl suite a) = do st <- get
                                ccl <- cExpCl cl
                                csuite <- lStmt suite
                                put st
                                return (Handler ccl csuite a) --no deberia cambiar nada

cExpCl :: (Arbitrary a, Eq a) => ExceptClause a -> VState a (ExceptClause a)
cExpCl e@(ExceptClause cl a) = case cl of
                                  Nothing -> return e
                                  Just (exp, m) -> do cexp <- cExpr exp
                                                      case m of
                                                         Nothing -> return (ExceptClause (Just (cexp, Nothing)) a)
                                                         Just exp2 -> do cexp2 <- cExpr exp2
                                                                         return (ExceptClause (Just (cexp, Just cexp2)) a)
testModule :: MPy
testModule = Module [stmt1, stmt2, stmt3, stmt4]
stmt1, stmt2, stmt3, stmt4 :: Statement ()
stmt1 = Assign [Var (Ident "a" () ) () ] (Int 1 "1" () ) ()
stmt2 = Assign [Var (Ident "b" () ) () ] (Int 2 "2" () ) ()
stmt3 = Assign [Var (Ident "c" () ) () ] (Int 3 "3" () ) ()
stmt4 = Assign [Var (Ident "d" () ) () ] (BinaryOp (Plus () ) (Var (Ident "x" () ) () ) (BinaryOp (Plus () ) (Var (Ident "y" () ) () ) (BinaryOp (Plus () ) (Var (Ident "z" () ) () ) (Var (Ident "x" () ) () ) () ) () ) () ) ()

ej :: IO ()
ej = let encoded = mencode testModule in
         do LC8.writeFile "ej.py" encoded
            newM <- generate $ cModule testModule
            let nencoded = mencode newM
            LC8.writeFile "nej.py" nencoded     
