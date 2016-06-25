{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances#-}

module Python where

import Test.QuickCheck
import Language.Python.Common
import Language.Python.Common.Pretty

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC8

import DeriveArbitrary
import Strings 

type MPy = Module ()

{-$(devArbitrary ''MPy)-}

instance Arbitrary annot_arpf => Arbitrary (Ident annot_arpf) where
      arbitrary
        = sized go_arvR
        where
            go_arvR n_arvS
              | (n_arvS <= 1)
              = oneof
                  [Ident <$> resize n_arvS arbitrary <*> resize n_arvS arbitrary]
              | otherwise
              = oneof
                  ([Ident <$> resize n_arvS arbitrary <*> resize n_arvS arbitrary]
                   ++ [Ident <$> resize n_arvS arbitrary <*> resize n_arvS arbitrary])
instance Arbitrary annot_arr8 => Arbitrary (Slice annot_arr8) where
      arbitrary
        = sized go_arvT
        where
            go_arvT n_arvU
              | (n_arvU <= 1)
              = oneof
                  [SliceProper <$> resize n_arvU arbitrary
                   <*> resize n_arvU arbitrary
                   <*> resize n_arvU arbitrary
                   <*> resize n_arvU arbitrary,
                   SliceExpr <$> resize n_arvU arbitrary <*> resize n_arvU arbitrary,
                   SliceEllipsis <$> resize n_arvU arbitrary]
              | otherwise
              = oneof
                  ([SliceProper <$> resize n_arvU arbitrary
                    <*> resize n_arvU arbitrary
                    <*> resize n_arvU arbitrary
                    <*> resize n_arvU arbitrary,
                    SliceExpr <$> resize n_arvU arbitrary <*> resize n_arvU arbitrary,
                    SliceEllipsis <$> resize n_arvU arbitrary]
                   ++
                     [SliceProper <$> resize n_arvU arbitrary
                      <*> resize n_arvU arbitrary
                      <*> resize n_arvU arbitrary
                      <*> resize n_arvU arbitrary,
                      SliceExpr <$> resize n_arvU arbitrary <*> resize n_arvU arbitrary,
                      SliceEllipsis <$> resize n_arvU arbitrary])
instance Arbitrary annot_arrl => Arbitrary (Op annot_arrl) where
      arbitrary
        = sized go_arvV
        where
            go_arvV n_arvW
              | (n_arvW <= 1)
              = oneof
                  [And <$> resize n_arvW arbitrary, Or <$> resize n_arvW arbitrary,
                   Not <$> resize n_arvW arbitrary,
                   Exponent <$> resize n_arvW arbitrary,
                   LessThan <$> resize n_arvW arbitrary,
                   GreaterThan <$> resize n_arvW arbitrary,
                   Equality <$> resize n_arvW arbitrary,
                   GreaterThanEquals <$> resize n_arvW arbitrary,
                   LessThanEquals <$> resize n_arvW arbitrary,
                   NotEquals <$> resize n_arvW arbitrary,
                   NotEqualsV2 <$> resize n_arvW arbitrary,
                   In <$> resize n_arvW arbitrary, Is <$> resize n_arvW arbitrary,
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
              | otherwise
              = oneof
                  ([And <$> resize n_arvW arbitrary, Or <$> resize n_arvW arbitrary,
                    Not <$> resize n_arvW arbitrary,
                    Exponent <$> resize n_arvW arbitrary,
                    LessThan <$> resize n_arvW arbitrary,
                    GreaterThan <$> resize n_arvW arbitrary,
                    Equality <$> resize n_arvW arbitrary,
                    GreaterThanEquals <$> resize n_arvW arbitrary,
                    LessThanEquals <$> resize n_arvW arbitrary,
                    NotEquals <$> resize n_arvW arbitrary,
                    NotEqualsV2 <$> resize n_arvW arbitrary,
                    In <$> resize n_arvW arbitrary, Is <$> resize n_arvW arbitrary,
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
                   ++
                     [And <$> resize n_arvW arbitrary, Or <$> resize n_arvW arbitrary,
                      Not <$> resize n_arvW arbitrary,
                      Exponent <$> resize n_arvW arbitrary,
                      LessThan <$> resize n_arvW arbitrary,
                      GreaterThan <$> resize n_arvW arbitrary,
                      Equality <$> resize n_arvW arbitrary,
                      GreaterThanEquals <$> resize n_arvW arbitrary,
                      LessThanEquals <$> resize n_arvW arbitrary,
                      NotEquals <$> resize n_arvW arbitrary,
                      NotEqualsV2 <$> resize n_arvW arbitrary,
                      In <$> resize n_arvW arbitrary, Is <$> resize n_arvW arbitrary,
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
                      Modulo <$> resize n_arvW arbitrary])
instance Arbitrary annot_artp =>
             Arbitrary (ParamTuple annot_artp) where
      arbitrary
        = sized go_arvX
        where
            go_arvX n_arvY
              | (n_arvY <= 1)
              = oneof
                  [ParamTupleName <$> resize n_arvY arbitrary
                   <*> resize n_arvY arbitrary]
              | otherwise
              = oneof
                  ([ParamTupleName <$> resize n_arvY arbitrary
                    <*> resize n_arvY arbitrary]
                   ++
                     [ParamTupleName <$> resize n_arvY arbitrary
                      <*> resize n_arvY arbitrary,
                      ParamTuple <$> (listOf $ (resize (n_arvY `div` 10) arbitrary))
                      <*> resize n_arvY arbitrary])
instance Arbitrary annot_arpj =>
             Arbitrary (Parameter annot_arpj) where
      arbitrary
        = sized go_arvZ
        where
            go_arvZ n_arw0
              | (n_arw0 <= 1)
              = oneof
                  [Param <$> resize n_arw0 arbitrary <*> resize n_arw0 arbitrary
                   <*> resize n_arw0 arbitrary
                   <*> resize n_arw0 arbitrary,
                   VarArgsPos <$> resize n_arw0 arbitrary <*> resize n_arw0 arbitrary
                   <*> resize n_arw0 arbitrary,
                   VarArgsKeyword <$> resize n_arw0 arbitrary
                   <*> resize n_arw0 arbitrary
                   <*> resize n_arw0 arbitrary,
                   EndPositional <$> resize n_arw0 arbitrary,
                   UnPackTuple <$> resize n_arw0 arbitrary <*> resize n_arw0 arbitrary
                   <*> resize n_arw0 arbitrary]
              | otherwise
              = oneof
                  ([Param <$> resize n_arw0 arbitrary <*> resize n_arw0 arbitrary
                    <*> resize n_arw0 arbitrary
                    <*> resize n_arw0 arbitrary,
                    VarArgsPos <$> resize n_arw0 arbitrary <*> resize n_arw0 arbitrary
                    <*> resize n_arw0 arbitrary,
                    VarArgsKeyword <$> resize n_arw0 arbitrary
                    <*> resize n_arw0 arbitrary
                    <*> resize n_arw0 arbitrary,
                    EndPositional <$> resize n_arw0 arbitrary,
                    UnPackTuple <$> resize n_arw0 arbitrary <*> resize n_arw0 arbitrary
                    <*> resize n_arw0 arbitrary]
                   ++
                     [Param <$> resize n_arw0 arbitrary <*> resize n_arw0 arbitrary
                      <*> resize n_arw0 arbitrary
                      <*> resize n_arw0 arbitrary,
                      VarArgsPos <$> resize n_arw0 arbitrary <*> resize n_arw0 arbitrary
                      <*> resize n_arw0 arbitrary,
                      VarArgsKeyword <$> resize n_arw0 arbitrary
                      <*> resize n_arw0 arbitrary
                      <*> resize n_arw0 arbitrary,
                      EndPositional <$> resize n_arw0 arbitrary,
                      UnPackTuple <$> resize n_arw0 arbitrary <*> resize n_arw0 arbitrary
                      <*> resize n_arw0 arbitrary])
instance Arbitrary annot_art6 => Arbitrary (YieldArg annot_art6) where
      arbitrary
        = sized go_arw1
        where
            go_arw1 n_arw2
              | (n_arw2 <= 1)
              = oneof
                  [YieldFrom <$> resize n_arw2 arbitrary <*> resize n_arw2 arbitrary,
                   YieldExpr <$> resize n_arw2 arbitrary]
              | otherwise
              = oneof
                  ([YieldFrom <$> resize n_arw2 arbitrary
                    <*> resize n_arw2 arbitrary,
                    YieldExpr <$> resize n_arw2 arbitrary]
                   ++
                     [YieldFrom <$> resize n_arw2 arbitrary <*> resize n_arw2 arbitrary,
                      YieldExpr <$> resize n_arw2 arbitrary])
instance Arbitrary annot_artk => Arbitrary (DictMappingPair annot_artk) where
      arbitrary
        = sized go_arw3
        where
            go_arw3 n_arw4
              | (n_arw4 <= 1)
              = oneof
                  [DictMappingPair <$> resize n_arw4 arbitrary
                   <*> resize n_arw4 arbitrary]
              | otherwise
              = oneof
                  ([DictMappingPair <$> resize n_arw4 arbitrary
                    <*> resize n_arw4 arbitrary]
                   ++
                     [DictMappingPair <$> resize n_arw4 arbitrary
                      <*> resize n_arw4 arbitrary])
instance Arbitrary annot_arty =>
             Arbitrary (ComprehensionExpr annot_arty) where
      arbitrary
        = sized go_arw5
        where
            go_arw5 n_arw6
              | (n_arw6 <= 1)
              = oneof
                  [ComprehensionExpr <$> resize n_arw6 arbitrary,
                   ComprehensionDict <$> resize n_arw6 arbitrary]
              | otherwise
              = oneof
                  ([ComprehensionExpr <$> resize n_arw6 arbitrary,
                    ComprehensionDict <$> resize n_arw6 arbitrary]
                   ++
                     [ComprehensionExpr <$> resize n_arw6 arbitrary,
                      ComprehensionDict <$> resize n_arw6 arbitrary])
instance Arbitrary annot_artV =>
             Arbitrary (CompIf annot_artV) where
      arbitrary
        = sized go_arw7
        where
            go_arw7 n_arw8
              | (n_arw8 <= 1)
              = oneof
                  [CompIf <$> resize n_arw8 arbitrary <*> resize n_arw8 arbitrary
                   <*> resize n_arw8 arbitrary]
              | otherwise
              = oneof
                  ([CompIf <$> resize n_arw8 arbitrary <*> resize n_arw8 arbitrary
                    <*> resize n_arw8 arbitrary]
                   ++
                     [CompIf <$> resize n_arw8 arbitrary <*> resize n_arw8 arbitrary
                      <*> resize n_arw8 arbitrary])
instance Arbitrary annot_artM =>
             Arbitrary (CompIter annot_artM) where
      arbitrary
        = sized go_arw9
        where
            go_arw9 n_arwa
              | (n_arwa <= 1)
              = oneof
                  [IterFor <$> resize n_arwa arbitrary <*> resize n_arwa arbitrary,
                   IterIf <$> resize n_arwa arbitrary <*> resize n_arwa arbitrary]
              | otherwise
              = oneof
                  ([IterFor <$> resize n_arwa arbitrary <*> resize n_arwa arbitrary,
                    IterIf <$> resize n_arwa arbitrary <*> resize n_arwa arbitrary]
                   ++
                     [IterFor <$> resize n_arwa arbitrary <*> resize n_arwa arbitrary,
                      IterIf <$> resize n_arwa arbitrary <*> resize n_arwa arbitrary])
instance Arbitrary annot_artH =>
             Arbitrary (CompFor annot_artH) where
      arbitrary
        = sized go_arwb
        where
            go_arwb n_arwc
              | (n_arwc <= 1)
              = oneof
                  [CompFor <$> (listOf $ (resize (n_arwc `div` 10) arbitrary))
                   <*> resize n_arwc arbitrary
                   <*> resize n_arwc arbitrary
                   <*> resize n_arwc arbitrary]
              | otherwise
              = oneof
                  ([CompFor <$> (listOf $ (resize (n_arwc `div` 10) arbitrary))
                    <*> resize n_arwc arbitrary
                    <*> resize n_arwc arbitrary
                    <*> resize n_arwc arbitrary]
                   ++
                     [CompFor <$> (listOf $ (resize (n_arwc `div` 10) arbitrary))
                      <*> resize n_arwc arbitrary
                      <*> resize n_arwc arbitrary
                      <*> resize n_arwc arbitrary])
instance Arbitrary annot_artf =>
             Arbitrary (Comprehension annot_artf) where
      arbitrary
        = sized go_arwd
        where
            go_arwd n_arwe
              | (n_arwe <= 1)
              = oneof
                  [Comprehension <$> resize n_arwe arbitrary
                   <*> resize n_arwe arbitrary
                   <*> resize n_arwe arbitrary]
              | otherwise
              = oneof
                  ([Comprehension <$> resize n_arwe arbitrary
                    <*> resize n_arwe arbitrary
                    <*> resize n_arwe arbitrary]
                   ++
                     [Comprehension <$> resize n_arwe arbitrary
                      <*> resize n_arwe arbitrary
                      <*> resize n_arwe arbitrary])
instance Arbitrary annot_arnc => Arbitrary (Expr annot_arnc) where
      arbitrary
        = sized go_arwf
        where
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
              = oneof
                  ([Var <$> resize n_arwg arbitrary <*> resize n_arwg arbitrary,
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
                   ++
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
                      Call <$> go_arwf (n_arwg - 1)
                      <*> (listOf $ (resize (n_arwg `div` 10) arbitrary))
                      <*> resize n_arwg arbitrary,
                      Subscript <$> go_arwf (n_arwg `div` 2) <*> go_arwf (n_arwg `div` 2)
                      <*> resize n_arwg arbitrary,
                      SlicedExpr <$> go_arwf (n_arwg - 1)
                      <*> (listOf $ (resize (n_arwg `div` 10) arbitrary))
                      <*> resize n_arwg arbitrary,
                      CondExpr <$> go_arwf (n_arwg `div` 3) <*> go_arwf (n_arwg `div` 3)
                      <*> go_arwf (n_arwg `div` 3)
                      <*> resize n_arwg arbitrary,
                      BinaryOp <$> resize n_arwg arbitrary <*> go_arwf (n_arwg `div` 2)
                      <*> go_arwf (n_arwg `div` 2)
                      <*> resize n_arwg arbitrary,
                      UnaryOp <$> resize n_arwg arbitrary <*> go_arwf (n_arwg - 1)
                      <*> resize n_arwg arbitrary,
                      Dot <$> go_arwf (n_arwg - 1) <*> resize n_arwg arbitrary
                      <*> resize n_arwg arbitrary,
                      Lambda <$> (listOf $ (resize (n_arwg `div` 10) arbitrary))
                      <*> go_arwf (n_arwg - 1)
                      <*> resize n_arwg arbitrary,
                      Tuple <$> (listOf $ (resize (n_arwg `div` 10) arbitrary))
                      <*> resize n_arwg arbitrary,
                      Yield <$> resize n_arwg arbitrary <*> resize n_arwg arbitrary,
                      Generator <$> resize n_arwg arbitrary <*> resize n_arwg arbitrary,
                      {-ListComp <$> resize n_arwg arbitrary <*> resize n_arwg arbitrary,-}
                      List <$> (listOf $ (resize (n_arwg `div` 10) arbitrary))
                      <*> resize n_arwg arbitrary,
                      Dictionary <$> (listOf $ (resize (n_arwg `div` 10) arbitrary))
                      <*> resize n_arwg arbitrary,
                      {-DictComp <$> resize n_arwg arbitrary <*> resize n_arwg arbitrary,-}
                      Set <$> (listOf $ (resize (n_arwg `div` 10) arbitrary))
                      <*> resize n_arwg arbitrary,
                      {-SetComp <$> resize n_arwg arbitrary <*> resize n_arwg arbitrary,-}
                      Starred <$> go_arwf (n_arwg - 1) <*> resize n_arwg arbitrary,
                      Paren <$> go_arwf (n_arwg - 1) <*> resize n_arwg arbitrary,
                      StringConversion <$> go_arwf (n_arwg - 1) <*> resize n_arwg arbitrary])
instance Arbitrary annot_arpE =>
             Arbitrary (Argument annot_arpE) where
      arbitrary
        = sized go_arwh
        where
            go_arwh n_arwi
              | (n_arwi <= 1)
              = oneof
                  [ArgExpr <$> resize n_arwi arbitrary <*> resize n_arwi arbitrary,
                   ArgVarArgsPos <$> resize n_arwi arbitrary
                   <*> resize n_arwi arbitrary,
                   ArgVarArgsKeyword <$> resize n_arwi arbitrary
                   <*> resize n_arwi arbitrary,
                   ArgKeyword <$> resize n_arwi arbitrary <*> resize n_arwi arbitrary
                   <*> resize n_arwi arbitrary]
              | otherwise
              = oneof
                  ([ArgExpr <$> resize n_arwi arbitrary <*> resize n_arwi arbitrary,
                    ArgVarArgsPos <$> resize n_arwi arbitrary
                    <*> resize n_arwi arbitrary,
                    ArgVarArgsKeyword <$> resize n_arwi arbitrary
                    <*> resize n_arwi arbitrary,
                    ArgKeyword <$> resize n_arwi arbitrary <*> resize n_arwi arbitrary
                    <*> resize n_arwi arbitrary]
                   ++
                     [ArgExpr <$> resize n_arwi arbitrary <*> resize n_arwi arbitrary,
                      ArgVarArgsPos <$> resize n_arwi arbitrary
                      <*> resize n_arwi arbitrary,
                      ArgVarArgsKeyword <$> resize n_arwi arbitrary
                      <*> resize n_arwi arbitrary,
                      ArgKeyword <$> resize n_arwi arbitrary <*> resize n_arwi arbitrary
                      <*> resize n_arwi arbitrary])
instance Arbitrary annot_arpV =>
             Arbitrary (AssignOp annot_arpV) where
      arbitrary
        = sized go_arwj
        where
            go_arwj n_arwk
              | (n_arwk <= 1)
              = oneof
                  [PlusAssign <$> resize n_arwk arbitrary,
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
              | otherwise
              = oneof
                  ([PlusAssign <$> resize n_arwk arbitrary,
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
                   ++
                     [PlusAssign <$> resize n_arwk arbitrary,
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
                      FloorDivAssign <$> resize n_arwk arbitrary])
instance Arbitrary annot_arqI =>
             Arbitrary (Decorator annot_arqI) where
      arbitrary
        = sized go_arwl
        where
            go_arwl n_arwm
              | (n_arwm <= 1)
              = oneof
                  [Decorator <$> resize n_arwm arbitrary
                   <*> (listOf $ (resize (n_arwm `div` 10) arbitrary))
                   <*> resize n_arwm arbitrary]
              | otherwise
              = oneof
                  ([Decorator <$> resize n_arwm arbitrary
                    <*> (listOf $ (resize (n_arwm `div` 10) arbitrary))
                    <*> resize n_arwm arbitrary]
                   ++
                     [Decorator <$> resize n_arwm arbitrary
                      <*> (listOf $ (resize (n_arwm `div` 10) arbitrary))
                      <*> resize n_arwm arbitrary])
instance Arbitrary annot_aru0 =>
             Arbitrary (ExceptClause annot_aru0) where
      arbitrary
        = sized go_arwn
        where
            go_arwn n_arwo
              | (n_arwo <= 1)
              = oneof
                  [ExceptClause <$> resize n_arwo arbitrary
                   <*> resize n_arwo arbitrary]
              | otherwise
              = oneof
                  ([ExceptClause <$> resize n_arwo arbitrary
                    <*> resize n_arwo arbitrary]
                   ++
                     [ExceptClause <$> resize n_arwo arbitrary
                      <*> resize n_arwo arbitrary])
instance Arbitrary annot_arr3 =>
             Arbitrary (FromItem annot_arr3) where
      arbitrary
        = sized go_arwp
        where
            go_arwp n_arwq
              | (n_arwq <= 1)
              = oneof
                  [FromItem <$> resize n_arwq arbitrary <*> resize n_arwq arbitrary
                   <*> resize n_arwq arbitrary]
              | otherwise
              = oneof
                  ([FromItem <$> resize n_arwq arbitrary <*> resize n_arwq arbitrary
                    <*> resize n_arwq arbitrary]
                   ++
                     [FromItem <$> resize n_arwq arbitrary <*> resize n_arwq arbitrary
                      <*> resize n_arwq arbitrary])
instance Arbitrary annot_arn3 =>
             Arbitrary (FromItems annot_arn3) where
      arbitrary
        = sized go_arwr
        where
            go_arwr n_arws
              | (n_arws <= 1)
              = oneof
                  [ImportEverything <$> resize n_arws arbitrary,
                   FromItems <$> (listOf $ (resize (n_arws `div` 10) arbitrary))
                   <*> resize n_arws arbitrary]
              | otherwise
              = oneof
                  ([ImportEverything <$> resize n_arws arbitrary,
                    FromItems <$> (listOf $ (resize (n_arws `div` 10) arbitrary))
                    <*> resize n_arws arbitrary]
                   ++
                     [ImportEverything <$> resize n_arws arbitrary,
                      FromItems <$> (listOf $ (resize (n_arws `div` 10) arbitrary))
                      <*> resize n_arws arbitrary])
instance Arbitrary annot_armT =>
             Arbitrary (ImportItem annot_armT) where
      arbitrary
        = sized go_arwt
        where
            go_arwt n_arwu
              | (n_arwu <= 1)
              = oneof
                  [ImportItem <$> resize n_arwu arbitrary <*> resize n_arwu arbitrary
                   <*> resize n_arwu arbitrary]
              | otherwise
              = oneof
                  ([ImportItem <$> resize n_arwu arbitrary
                    <*> resize n_arwu arbitrary
                    <*> resize n_arwu arbitrary]
                   ++
                     [ImportItem <$> resize n_arwu arbitrary <*> resize n_arwu arbitrary
                      <*> resize n_arwu arbitrary])
instance Arbitrary annot_armY =>
             Arbitrary (ImportRelative annot_armY) where
      arbitrary
        = sized go_arwv
        where
            go_arwv n_arww
              | (n_arww <= 1)
              = oneof
                  [ImportRelative <$> resize n_arww arbitrary
                   <*> resize n_arww arbitrary
                   <*> resize n_arww arbitrary]
              | otherwise
              = oneof
                  ([ImportRelative <$> resize n_arww arbitrary
                    <*> resize n_arww arbitrary
                    <*> resize n_arww arbitrary]
                   ++
                     [ImportRelative <$> resize n_arww arbitrary
                      <*> resize n_arww arbitrary
                      <*> resize n_arww arbitrary])
instance Arbitrary annot_arqS =>
             Arbitrary (RaiseExpr annot_arqS) where
      arbitrary
        = sized go_arwx
        where
            go_arwx n_arwy
              | (n_arwy <= 1)
              = oneof
                  [RaiseV3 <$> resize n_arwy arbitrary,
                   RaiseV2 <$> resize n_arwy arbitrary]
              | otherwise
              = oneof
                  ([RaiseV3 <$> resize n_arwy arbitrary,
                    RaiseV2 <$> resize n_arwy arbitrary]
                   ++
                     [RaiseV3 <$> resize n_arwy arbitrary,
                      RaiseV2 <$> resize n_arwy arbitrary])
instance Arbitrary annot_arlk =>
             Arbitrary (Statement annot_arlk) where
      arbitrary
        = sized go_arwz
        where
            go_arwz n_arwA
              | (n_arwA <= 1)
              = oneof
                  [Import <$> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                   <*> resize n_arwA arbitrary,
                   FromImport <$> resize n_arwA arbitrary <*> resize n_arwA arbitrary
                   <*> resize n_arwA arbitrary,
                   While <$> resize n_arwA arbitrary <*> resize n_arwA arbitrary
                   <*> resize n_arwA arbitrary
                   <*> resize n_arwA arbitrary,
                   For <$> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                   <*> resize n_arwA arbitrary
                   <*> resize n_arwA arbitrary
                   <*> resize n_arwA arbitrary
                   <*> resize n_arwA arbitrary,
                   Fun <$> resize n_arwA arbitrary
                   <*> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                   <*> resize n_arwA arbitrary
                   <*> resize n_arwA arbitrary
                   <*> resize n_arwA arbitrary,
                   Class <$> resize n_arwA arbitrary
                   <*> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                   <*> resize n_arwA arbitrary
                   <*> resize n_arwA arbitrary,
                   Conditional <$> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                   <*> resize n_arwA arbitrary
                   <*> resize n_arwA arbitrary,
                   Assign <$> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                   <*> resize n_arwA arbitrary
                   <*> resize n_arwA arbitrary,
                   AugmentedAssign <$> resize n_arwA arbitrary
                   <*> resize n_arwA arbitrary
                   <*> resize n_arwA arbitrary
                   <*> resize n_arwA arbitrary,
                   Return <$> resize n_arwA arbitrary <*> resize n_arwA arbitrary,
                   Try <$> resize n_arwA arbitrary
                   <*> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                   <*> resize n_arwA arbitrary
                   <*> resize n_arwA arbitrary
                   <*> resize n_arwA arbitrary,
                   Raise <$> resize n_arwA arbitrary <*> resize n_arwA arbitrary,
                   With <$> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                   <*> resize n_arwA arbitrary
                   <*> resize n_arwA arbitrary,
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
                   Print <$> resize n_arwA arbitrary
                   <*> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                   <*> resize n_arwA arbitrary
                   <*> resize n_arwA arbitrary,
                   Exec <$> resize n_arwA arbitrary <*> resize n_arwA arbitrary
                   <*> resize n_arwA arbitrary]
              | otherwise
              = oneof
                  ([Import <$> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                    <*> resize n_arwA arbitrary,
                    FromImport <$> resize n_arwA arbitrary <*> resize n_arwA arbitrary
                    <*> resize n_arwA arbitrary,
                    While <$> resize n_arwA arbitrary <*> resize n_arwA arbitrary
                    <*> resize n_arwA arbitrary
                    <*> resize n_arwA arbitrary,
                    For <$> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                    <*> resize n_arwA arbitrary
                    <*> resize n_arwA arbitrary
                    <*> resize n_arwA arbitrary
                    <*> resize n_arwA arbitrary,
                    Fun <$> resize n_arwA arbitrary
                    <*> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                    <*> resize n_arwA arbitrary
                    <*> resize n_arwA arbitrary
                    <*> resize n_arwA arbitrary,
                    Class <$> resize n_arwA arbitrary
                    <*> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                    <*> resize n_arwA arbitrary
                    <*> resize n_arwA arbitrary,
                    Conditional <$> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                    <*> resize n_arwA arbitrary
                    <*> resize n_arwA arbitrary,
                    Assign <$> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                    <*> resize n_arwA arbitrary
                    <*> resize n_arwA arbitrary,
                    AugmentedAssign <$> resize n_arwA arbitrary
                    <*> resize n_arwA arbitrary
                    <*> resize n_arwA arbitrary
                    <*> resize n_arwA arbitrary,
                    Return <$> resize n_arwA arbitrary <*> resize n_arwA arbitrary,
                    Try <$> resize n_arwA arbitrary
                    <*> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                    <*> resize n_arwA arbitrary
                    <*> resize n_arwA arbitrary
                    <*> resize n_arwA arbitrary,
                    Raise <$> resize n_arwA arbitrary <*> resize n_arwA arbitrary,
                    With <$> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                    <*> resize n_arwA arbitrary
                    <*> resize n_arwA arbitrary,
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
                    Print <$> resize n_arwA arbitrary
                    <*> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                    <*> resize n_arwA arbitrary
                    <*> resize n_arwA arbitrary,
                    Exec <$> resize n_arwA arbitrary <*> resize n_arwA arbitrary
                    <*> resize n_arwA arbitrary]
                   ++
                     [Import <$> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                      <*> resize n_arwA arbitrary,
                      FromImport <$> resize n_arwA arbitrary <*> resize n_arwA arbitrary
                      <*> resize n_arwA arbitrary,
                      While <$> resize n_arwA arbitrary <*> resize n_arwA arbitrary
                      <*> resize n_arwA arbitrary
                      <*> resize n_arwA arbitrary,
                      For <$> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                      <*> resize n_arwA arbitrary
                      <*> resize n_arwA arbitrary
                      <*> resize n_arwA arbitrary
                      <*> resize n_arwA arbitrary,
                      Fun <$> resize n_arwA arbitrary
                      <*> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                      <*> resize n_arwA arbitrary
                      <*> resize n_arwA arbitrary
                      <*> resize n_arwA arbitrary,
                      Class <$> resize n_arwA arbitrary
                      <*> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                      <*> resize n_arwA arbitrary
                      <*> resize n_arwA arbitrary,
                      Conditional <$> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                      <*> resize n_arwA arbitrary
                      <*> resize n_arwA arbitrary,
                      Assign <$> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                      <*> resize n_arwA arbitrary
                      <*> resize n_arwA arbitrary,
                      AugmentedAssign <$> resize n_arwA arbitrary
                      <*> resize n_arwA arbitrary
                      <*> resize n_arwA arbitrary
                      <*> resize n_arwA arbitrary,
                      Decorated <$> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                      <*> go_arwz (n_arwA - 1)
                      <*> resize n_arwA arbitrary,
                      Return <$> resize n_arwA arbitrary <*> resize n_arwA arbitrary,
                      Try <$> resize n_arwA arbitrary
                      <*> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                      <*> resize n_arwA arbitrary
                      <*> resize n_arwA arbitrary
                      <*> resize n_arwA arbitrary,
                      Raise <$> resize n_arwA arbitrary <*> resize n_arwA arbitrary,
                      With <$> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                      <*> resize n_arwA arbitrary
                      <*> resize n_arwA arbitrary,
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
                      Print <$> resize n_arwA arbitrary
                      <*> (listOf $ (resize (n_arwA `div` 10) arbitrary))
                      <*> resize n_arwA arbitrary
                      <*> resize n_arwA arbitrary,
                      Exec <$> resize n_arwA arbitrary <*> resize n_arwA arbitrary
                      <*> resize n_arwA arbitrary])
instance Arbitrary annot_arqN =>
             Arbitrary (Handler annot_arqN) where
      arbitrary
        = sized go_arwB
        where
            go_arwB n_arwC
              | (n_arwC <= 1)
              = oneof
                  [Handler <$> resize n_arwC arbitrary <*> resize n_arwC arbitrary
                   <*> resize n_arwC arbitrary]
              | otherwise
              = oneof
                  ([Handler <$> resize n_arwC arbitrary <*> resize n_arwC arbitrary
                    <*> resize n_arwC arbitrary]
                   ++
                     [Handler <$> resize n_arwC arbitrary <*> resize n_arwC arbitrary
                      <*> resize n_arwC arbitrary])
instance Arbitrary annot_arbb =>
             Arbitrary (Module annot_arbb) where
      arbitrary
        = sized go_arwD
        where
            go_arwD n_arwE
              | (n_arwE <= 1)
              = oneof
                  [Module <$> (listOf $ (resize (n_arwE `div` 10) arbitrary))]
              | otherwise
              = oneof
                  [Module <$> (listOf $ (resize (n_arwE `div` 10) arbitrary))]

mencode :: MPy -> LC8.ByteString
mencode x = LC8.pack $ prettyText x
