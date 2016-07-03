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
import Data.Generics.Str
import Data.Generics.Uniplate.Data
import Data.Generics.Uniplate.Operations
import Control.Monad

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

--maybe add random char instead of _
identifierFix :: String -> String
identifierFix s = let fixStart c = if isValidStart c then c else '_'
                      fixContinue c = if isValidContinue c then c else '_'
                  in case s of
                    ""      -> "_"
                    (st:c)  -> let fixed = (fixStart st):(map fixContinue c)
                               in if isReservedWord fixed then '_':fixed else fixed

type MPy = Module ()

{-$(devArbitrary ''MPy)-}

instance Arbitrary a => Arbitrary (Ident a) where --"fixed" instance
      arbitrary = sized go_arvR where
            go_arvR n_arvS = Ident <$> liftM identifierFix (resize n_arvS arbitrary) <*> resize n_arvS arbitrary  
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
                   Generator <$> resize n_arwg arbitrary <*> resize n_arwg arbitrary,
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
                           (2, Generator <$> resize n_arwg arbitrary <*> resize n_arwg arbitrary),
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
instance Arbitrary a => Arbitrary (Module a) where
      arbitrary = sized go_arwD where
            go_arwD n_arwE = Module <$> (listOf $ (resize (n_arwE `div` 10) arbitrary))

mencode :: MPy -> LC8.ByteString
mencode x = LC8.pack $ prettyText x
