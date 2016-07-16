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
class Fixable a where
  fixUp :: a -> Gen a

instance Arbitrary a => Fixable (Ident a) where
  fixUp = readableFix --return . identifierFix

start :: [Char]
start = ['a'..'z'] ++ ['A'..'Z'] ++ ['_']

cont :: [Char]
cont = start ++ ['0'..'9']

readableFix :: Arbitrary a => Ident a -> Gen (Ident a)
readableFix id = do s <- elements start
                    c <- shuffle cont
                    let nid = s:(take 4 c)
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

type MPy = Module ()

{-$(devArbitrary ''MPy)-}

instance Arbitrary a => Arbitrary (Ident a) where --easy to read instance
      arbitrary = sized go >>= fixUp where
            go n = Ident <$> resize n arbitrary <*> resize n arbitrary  
instance Arbitrary a => Arbitrary (Slice a) where
      arbitrary = sized go where
            go n = oneof [SliceProper <$> resize n arbitrary
                                      <*> resize n arbitrary
                                      <*> resize n arbitrary
                                      <*> resize n arbitrary,
                          SliceExpr <$> resize n arbitrary <*> resize n arbitrary,
                          SliceEllipsis <$> resize n arbitrary]
instance Arbitrary a => Arbitrary (Op a) where
      arbitrary = sized go where
            go n = oneof [And <$> resize n arbitrary,
                          Or <$> resize n arbitrary,
                          Not <$> resize n arbitrary,
                          Exponent <$> resize n arbitrary,
                          LessThan <$> resize n arbitrary,
                          GreaterThan <$> resize n arbitrary,
                          Equality <$> resize n arbitrary,
                          GreaterThanEquals <$> resize n arbitrary,
                          LessThanEquals <$> resize n arbitrary,
                          NotEquals <$> resize n arbitrary,
                          NotEqualsV2 <$> resize n arbitrary,
                          In <$> resize n arbitrary,
                          Is <$> resize n arbitrary,
                          IsNot <$> resize n arbitrary,
                          NotIn <$> resize n arbitrary,
                          BinaryOr <$> resize n arbitrary,
                          Xor <$> resize n arbitrary,
                          BinaryAnd <$> resize n arbitrary,
                          ShiftLeft <$> resize n arbitrary,
                          ShiftRight <$> resize n arbitrary,
                          Multiply <$> resize n arbitrary,
                          Plus <$> resize n arbitrary,
                          Minus <$> resize n arbitrary,
                          Divide <$> resize n arbitrary,
                          FloorDivide <$> resize n arbitrary,
                          Invert <$> resize n arbitrary,
                          Modulo <$> resize n arbitrary]
instance Arbitrary a => Arbitrary (ParamTuple a) where
      arbitrary = sized go where
            go n | (n <= 1) = ParamTupleName <$> resize n arbitrary <*> resize n arbitrary
                 | otherwise = frequency [(2, ParamTupleName <$> resize n arbitrary
                                                             <*> resize n arbitrary),
                                          (1, ParamTuple <$> (listOf $ (resize (n `div` 10) arbitrary))
                                                         <*> resize n arbitrary)]
instance Arbitrary a => Arbitrary (Parameter a) where
      arbitrary = sized go where
            go n = oneof [Param <$> resize n arbitrary <*> resize n arbitrary
                                <*> resize n arbitrary <*> resize n arbitrary,
                          VarArgsPos <$> resize n arbitrary <*> resize n arbitrary
                                     <*> resize n arbitrary,
                          VarArgsKeyword <$> resize n arbitrary <*> resize n arbitrary
                                         <*> resize n arbitrary,
                          EndPositional <$> resize n arbitrary,
                          UnPackTuple <$> resize n arbitrary <*> resize n arbitrary
                                      <*> resize n arbitrary]
instance Arbitrary a => Arbitrary (YieldArg a) where
      arbitrary = sized go where
            go n = oneof [YieldFrom <$> resize n arbitrary <*> resize n arbitrary,
                          YieldExpr <$> resize n arbitrary]
instance Arbitrary a => Arbitrary (DictMappingPair a) where
      arbitrary = sized go where
            go n = DictMappingPair <$> resize n arbitrary <*> resize n arbitrary
instance Arbitrary a => Arbitrary (ComprehensionExpr a) where
      arbitrary = sized go where
            go n = oneof [ComprehensionExpr <$> resize n arbitrary,
                          ComprehensionDict <$> resize n arbitrary]
instance Arbitrary a => Arbitrary (CompIf a) where
      arbitrary = sized go where
            go n = CompIf <$> resize n arbitrary <*> resize n arbitrary
                          <*> resize n arbitrary
instance Arbitrary a => Arbitrary (CompIter a) where
      arbitrary = sized go where
            go n = oneof [IterFor <$> resize n arbitrary <*> resize n arbitrary,
                          IterIf <$> resize n arbitrary <*> resize n arbitrary]
instance Arbitrary a => Arbitrary (CompFor a) where
      arbitrary = sized go where
            go n = CompFor <$> (listOf $ (resize (n `div` 10) arbitrary))
                           <*> resize n arbitrary <*> resize n arbitrary
                           <*> resize n arbitrary
instance Arbitrary a => Arbitrary (Comprehension a) where
      arbitrary = sized go where
            go n = Comprehension <$> resize n arbitrary <*> resize n arbitrary
                                 <*> resize n arbitrary
instance Arbitrary a => Arbitrary (Expr a) where
      arbitrary = sized go where
            go n | (n <= 1) = oneof
                                [Var <$> resize n arbitrary <*> resize n arbitrary,
                                 Int <$> resize n arbitrary <*> resize n arbitrary
                                     <*> resize n arbitrary,
                                 LongInt <$> resize n arbitrary <*> resize n arbitrary
                                     <*> resize n arbitrary,
                                 Float <$> resize n arbitrary <*> resize n arbitrary
                                       <*> resize n arbitrary,
                                 Imaginary <$> resize n arbitrary <*> resize n arbitrary
                                           <*> resize n arbitrary,
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
                                       (2, Int <$> resize n arbitrary <*> resize n arbitrary
                                               <*> resize n arbitrary),
                                       (2, LongInt <$> resize n arbitrary <*> resize n arbitrary
                                                   <*> resize n arbitrary),
                                       (2, Float <$> resize n arbitrary <*> resize n arbitrary
                                                 <*> resize n arbitrary),
                                       (2, Imaginary <$> resize n arbitrary <*> resize n arbitrary
                                                     <*> resize n arbitrary),
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
instance Arbitrary a => Arbitrary (Argument a) where
      arbitrary = sized go where
            go n = oneof [ArgExpr <$> resize n arbitrary <*> resize n arbitrary,
                          ArgVarArgsPos <$> resize n arbitrary <*> resize n arbitrary,
                          ArgVarArgsKeyword <$> resize n arbitrary <*> resize n arbitrary,
                          ArgKeyword <$> resize n arbitrary <*> resize n arbitrary
                                     <*> resize n arbitrary]
instance Arbitrary a => Arbitrary (AssignOp a) where
      arbitrary = sized go where
            go n = oneof [PlusAssign <$> resize n arbitrary,
                          MinusAssign <$> resize n arbitrary,
                          MultAssign <$> resize n arbitrary,
                          DivAssign <$> resize n arbitrary,
                          ModAssign <$> resize n arbitrary,
                          PowAssign <$> resize n arbitrary,
                          BinAndAssign <$> resize n arbitrary,
                          BinOrAssign <$> resize n arbitrary,
                          BinXorAssign <$> resize n arbitrary,
                          LeftShiftAssign <$> resize n arbitrary,
                          RightShiftAssign <$> resize n arbitrary,
                          FloorDivAssign <$> resize n arbitrary]
instance Arbitrary a => Arbitrary (Decorator a) where
      arbitrary = sized go where
            go n = Decorator <$> resize n arbitrary
                             <*> (listOf $ (resize (n `div` 10) arbitrary))
                             <*> resize n arbitrary
instance Arbitrary a => Arbitrary (ExceptClause a) where
      arbitrary = sized go where
            go n = ExceptClause <$> resize n arbitrary <*> resize n arbitrary
instance Arbitrary a => Arbitrary (FromItem a) where
      arbitrary = sized go where
            go n = FromItem <$> resize n arbitrary <*> resize n arbitrary
                            <*> resize n arbitrary
instance Arbitrary a => Arbitrary (FromItems a) where
      arbitrary = sized go where
            go n = oneof [ImportEverything <$> resize n arbitrary,
                          FromItems <$> (listOf $ (resize (n `div` 10) arbitrary))
                                    <*> resize n arbitrary]
instance Arbitrary a => Arbitrary (ImportItem a) where
      arbitrary = sized go where
            go n = ImportItem <$> resize n arbitrary <*> resize n arbitrary
                              <*> resize n arbitrary
instance Arbitrary a => Arbitrary (ImportRelative a) where
      arbitrary = sized go where
            go n = ImportRelative <$> resize n arbitrary <*> resize n arbitrary
                                  <*> resize n arbitrary
instance Arbitrary a => Arbitrary (RaiseExpr a) where
      arbitrary = sized go where
            go n = oneof [RaiseV3 <$> resize n arbitrary,
                          RaiseV2 <$> resize n arbitrary]
instance Arbitrary a => Arbitrary (Statement a) where
      arbitrary = sized go where
            go n | (n <= 1) = oneof [Import <$> (listOf $ (resize (n `div` 10) arbitrary))
                                            <*> resize n arbitrary,
                                     FromImport <$> resize n arbitrary <*> resize n arbitrary
                                                <*> resize n arbitrary,
                                     While <$> resize n arbitrary <*> resize n arbitrary
                                           <*> resize n arbitrary <*> resize n arbitrary,
                                     For <$> (listOf $ (resize (n `div` 10) arbitrary))
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
                                     Assign <$> (listOf $ (resize (n `div` 10) arbitrary))
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
                                       (2, Assign <$> (listOf $ (resize (n `div` 10) arbitrary))
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
instance Arbitrary a => Arbitrary (Handler a) where
      arbitrary = sized go where
            go n = Handler <$> resize n arbitrary <*> resize n arbitrary
                           <*> resize n arbitrary

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

instance (Arbitrary a, Eq a) => Arbitrary (Module a) where
      {-arbitrary = sized go where
            go n_arwE = Module <$> (listOf $ (resize (n_arwE `div` 10) arbitrary))-}
  arbitrary = arbMod >>= cModule --we coherent now

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
      
--literal now shows number, needs fix to display the correct one
arbExpr :: Arbitrary a => Gen(Expr a) 
arbExpr = sized aux where
            aux n | (n <= 1) = oneof [Var <$> (resize n $ (custToGen IdVar)) <*> resize n arbitrary,
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
                                      Yield <$> (resize n $ arbMaybe (arbYield)) <*> resize n arbitrary,
                                      {-Generator <$> (resize n arbComp) <*> resize n arbitrary,-}
                                      {-ListComp <$> (resize n arbComp) <*> resize n arbitrary,-}
                                      Dictionary <$> (listOf $ (resize (n `div` 10) arbDMP))
                                                 <*> resize n arbitrary{-,
                                      DictComp <$> (resize n arbComp) <*> resize n arbitrary,
                                      SetComp <$> (resize n arbComp) <*> resize n arbitrary-}]
              | otherwise
              = frequency [(1, Var <$> (resize n $ (custToGen IdVar)) <*> resize n arbitrary),
                           (1, gInt True),
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
                                           {-Conditional <$> (listOf $ (resize (n `div` 10) $ liftM2 (,) arbExpr (listOf (resize (n `div` 10) arbStmt))))
                                                       <*> resize n arbitrary <*> resize n arbitrary,-}
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
                                               (20, Fun <$> resize n (custToGen IdFunc) <*> (listOf $ (resize (n `div` 10) arbParam))
                                                       <*> (resize n $ arbMaybe arbExpr) <*> (listOf (resize (n `div` 10) arbStmt))
                                                       <*> resize n arbitrary),
                                               (2, Class <$> resize n (custToGen IdClass) --class names?
                                                         <*> (listOf $ (resize (n `div` 10) arbArg))
                                                         <*> (listOf (resize (n `div` 10) arbStmt)) <*> resize n arbitrary),
                                               {-(2, Conditional <$> (listOf $ (resize (n `div` 10) $ liftM2 (,) arbExpr (listOf (resize (n `div` 10) arbStmt))))
                                                               <*> resize n arbitrary <*> resize n arbitrary),-}
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

--------------------------
----Coherent variables----
--------------------------

data StV a = StV { gvars :: [Ident a],
                   lvars :: [[Ident a]]} deriving Show

initV :: StV a
initV = StV [] [[]]

-- >>= :: VState a b -> (b -> VState a c) -> VState a c
type VState a b = StateT (StV a) Gen b

--Remove local scope
popLvars :: VState a ()
popLvars = do st <- get
              put $ st {lvars = tail $ lvars st}  

--Add scope
pushLvars :: [Ident a] -> VState a ()
pushLvars vs = do st <- get
                  put $ st {lvars = (vs ++ (head $ lvars st)):(lvars st)}

--Add vars to local scope
pushScope :: [Ident a] -> VState a ()
pushScope vs = do st <- get
                  put $ st {lvars = (vs ++ (head $ lvars st)):(tail $ lvars st)}

--Remove vars from local scope
popScope :: Eq a => Ident a -> VState a ()
popScope i = do st <- get
                put $ st {lvars = delete i (head $ lvars st):(lvars st)}

--for Del
delGvars :: Eq a => Ident a -> VState a ()
delGvars i = do st <- get
                put $ st {gvars = delete i (gvars st)} 

pushGvars :: [Ident a] -> VState a ()
pushGvars i = do st <- get
                 put $ st {gvars = i++(gvars st)}

createVar :: Arbitrary a => [Ident a] -> Gen (Expr a)
createVar xs = do Var <$> elements xs <*> arbitrary                  

--Module
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
                                     cbody <- lStmt body --warning: else will have body's scope
                                     cwelse <- lStmt welse
                                     return (While ccond cbody cwelse a)
cStmt (For [targ] gen body felse a) = let targ_id = var_ident targ in
                                          do st <- get
                                             case elem targ_id (gvars st) of --can't be global, generate another var
                                                True -> do new_id <- lift $ genId vars
                                                           new_targ <- lift $ createVar [new_id]
                                                           pushScope [var_ident new_targ]
                                                           cgen <- cExpr gen
                                                           cbody <- lStmt body
                                                           cfelse <- lStmt felse
                                                           return (For [new_targ] gen body felse a)
                                                False -> case elem targ_id (head $ lvars st) of --if it's local, good
                                                            True -> do cgen <- cExpr gen
                                                                       cbody <- lStmt body 
                                                                       cfelse <- lStmt felse
                                                                       return (For [(Var targ_id a)] cgen cbody cfelse a)
                                                            False -> do pushScope [targ_id] --add to local scope
                                                                        cgen <- cExpr gen 
                                                                        cbody <- lStmt body 
                                                                        cfelse <- lStmt felse
                                                                        return (For [(Var targ_id a)] cgen cbody cfelse a)
cStmt (Fun fname args annot body a) = let args_ids = map param_name args in
                                          do pushLvars args_ids  --fun args go to local scope
                                             cbody <- lStmt body
                                             popLvars --we remove args when leaving fun
                                             return (Fun fname args annot cbody a)
cStmt (Class cname args body a) = do st <- get
                                     cbody <- lStmt body
                                     put st
                                     return (Class cname args cbody a)
-- [(Expr, [Stmt])] --bimap--> [(VState Expr, VState [Stmt])] --auxM--> [VState (Expr, [Stmt])] --sequence--> VState [(Expr, [Stmt])]
cStmt (Conditional guards celse a) = do cguards <- sequence $ map (auxM . (bimap cExpr lStmt)) guards 
                                        ccelse <- lStmt celse    
                                        return (Conditional cguards ccelse a)
cStmt (Assign [to] ev a) = let to_id = var_ident to in
                           do st <- get
                              case elem to_id (gvars st) of
                                  True -> do cev <- cExpr ev
                                             return (Assign [to] cev a)
                                  False -> case elem to_id (head $ lvars st) of
                                              True -> do cev <- cExpr ev
                                                         return (Assign [to] cev a)
                                              False -> do pushScope [to_id] --add to local scope
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
                                                        False -> let pool = (gvars st) ++ (head $ lvars st) in --doesn't exists, we generate one from what we have
                                                                     do v <- lift $ createVar pool
                                                                        cev <- cExpr ev 
                                                                        return (AugmentedAssign v op cev a)
cStmt (Decorated decs fun a) = do st <- get
                                  cfun <- cStmt fun
                                  put st
                                  return (Decorated decs cfun a)
cStmt r@(Return ret a) = case ret of
                             Nothing -> return r
                             Just e -> do st <- get
                                          ce <- cExpr e
                                          put st
                                          return (Return (Just ce) a) 
cStmt (Try body exp telse fin a) = do cbody <- lStmt body
                                      cexp <- lHand exp
                                      ctelse <- lStmt telse
                                      cfin <- lStmt fin
                                      return (Try cbody cexp ctelse cfin a)
cStmt r@(Raise expr a) = return r
cStmt (With cont body a) = do cbody <- lStmt body 
                              return (With cont cbody a)
--we should delete var from scope, but we leave in order to not break conditionals
cStmt d@(Delete [expr] a) = let expr_id = var_ident expr in 
                             do st <- get
                                case elem expr_id (gvars st) of
                                  True -> {-delGvars expr_id >>-} return d
                                  False -> case elem expr_id (head $ lvars st) of
                                              True -> {-popScope expr_id >>-} return d
                                              False -> return (Pass a) --if it doesn't exists, replace for a Pass
cStmt (StmtExpr expr a) = do cexpr <- cExpr expr
                             return (StmtExpr cexpr a)
cStmt g@(Global idlist a) = pushGvars idlist >> return g
cStmt n@(NonLocal idlist a) = return n
cStmt (Assert exprl a) = do st <- get
                            cexpr <- lExpr exprl
                            put st
                            return (Assert cexpr a)
cStmt (Print c exprl t a) = do st <- get
                               cexpr <- lExpr exprl
                               put st                          
                               return (Print c cexpr t a)
cStmt (Exec expr env a) = do st <- get
                             cexpr <- cExpr expr
                             put st
                             return (Exec cexpr env a)
cStmt s = return s

--Expresions
lExpr :: (Arbitrary a, Eq a) => [Expr a] -> VState a [Expr a]
lExpr = mapM cExpr

cExpr :: (Arbitrary a, Eq a) => Expr a -> VState a (Expr a)
cExpr v@(Var id a) = do st <- get
                        case elem id (gvars st) of
                           True -> return v
                           False -> case elem id (head $ lvars st) of
                                        True -> return v
                                        False -> let pool = (gvars st) ++ (head $ lvars st) in --generate from what we have (todo - or assign a constant)
                                                   do newv <- lift $ createVar pool
                                                      return newv
cExpr c@(Call fun args a) = return c 
cExpr (Subscript subs expr a) = do csubs <- cExpr subs
                                   cexpr <- cExpr expr
                                   return (Subscript csubs cexpr a)
cExpr (SlicedExpr sle sls a) = do csle <- cExpr sle 
                                  return (SlicedExpr csle sls a)
cExpr (CondExpr true cond false a) = do ccond <- cExpr cond
                                        ctrue <- cExpr true
                                        cfalse <- cExpr false
                                        return (CondExpr ccond ctrue cfalse a)
cExpr (BinaryOp op e1 e2 a) = do ce1 <- cExpr e1
                                 ce2 <- cExpr e2
                                 return (BinaryOp op ce1 ce2 a)
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
                                return (Handler ccl csuite a) 

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
stmt2 = Assign [Var (Ident "b" () ) () ] (Var (Ident "x" () ) () ) ()
stmt3 = Assign [Var (Ident "c" () ) () ] (Int 3 "3" () ) ()
stmt4 = Assign [Var (Ident "d" () ) () ] (BinaryOp (Plus () ) (Var (Ident "x" () ) () ) (BinaryOp (Plus () ) (Var (Ident "y" () ) () ) (BinaryOp (Plus () ) (Var (Ident "z" () ) () ) (Var (Ident "x" () ) () ) () ) () ) () ) ()

ej :: IO ()
ej = let encoded = mencode testModule in
         do LC8.putStrLn encoded
            putStrLn ""
            newM <- generate $ cModule testModule
            let nencoded = mencode newM
            LC8.putStrLn nencoded   
