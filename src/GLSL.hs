{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts, IncoherentInstances, MultiParamTypeClasses, ConstraintKinds, ScopedTypeVariables, UndecidableInstances #-}

module GLSL where

import Test.QuickCheck
import Language.GLSL.Syntax
import Language.GLSL.Pretty
import Text.PrettyPrint.HughesPJClass
import Control.Monad.Trans
import Control.Monad.Trans.State

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC8

import Data.List
import DeriveArbitrary
import DeriveFixable
import Strings 

instance Arbitrary String where
   arbitrary = genName

initV :: StV (String)
initV = StV []

popId :: String -> VState String ()
popId i = do st <- get
             put $ st {vars = delete i (vars st)}

pushId :: String -> VState String ()
pushId i = do st <- get
              put $ st {vars = i:(vars st)}


getVId (Variable x) = x
getVId _ = ""

getAId (Equal x _) = getVId x
getAId _ = ""

genCons :: Gen Expr 
genCons = return $ IntConstant Decimal 42

genVar :: [String] -> Gen Expr
genVar xs = do n <- elements xs
               return (Variable n)

type MGLSL = TranslationUnit

-- $(devArbitrary ''MGLSL)

instance Arbitrary IntConstantKind where
      arbitrary
        = sized go_agjk
        where
            go_agjk n_agjl
              = oneof [return Hexadecimal, return Octal, return Decimal]

instance Arbitrary (Maybe PrecisionQualifier) where
      arbitrary = return Nothing


instance Arbitrary PrecisionQualifier where
      arbitrary
        = sized go_agjm
        where
            go_agjm n_agjn = oneof [return HighP, return MediumP, return LowP]

instance Arbitrary StorageQualifier where
      arbitrary
        = sized go_agjo
        where
            go_agjo n_agjp
              = oneof
                  [return Const, return Attribute , return Varying --,
                   {-return CentroidVarying, return In, return Out, return CentroidIn,
                   return CentroidOut-}, return Uniform]

instance Arbitrary LayoutQualifierId where
      arbitrary
        = sized go_agjq
        where
            go_agjq n_agjr
              = LayoutQualId <$> resize (max 0 (n_agjr - 1)) arbitrary
                <*> resize (max 0 (n_agjr - 1)) arbitrary

instance Arbitrary LayoutQualifier where
      arbitrary
        = sized go_agjs
        where
            go_agjs n_agjt
              = Layout <$> (listOf $ (resize (n_agjt `div` 10) arbitrary))

instance Arbitrary InterpolationQualifier where
      arbitrary
        = sized go_agju
        where
            go_agju n_agjv
              = oneof [return Smooth, return Flat, return NoPerspective]

instance Arbitrary InvariantQualifier where
      arbitrary
        = sized go_agjw
        where
            go_agjw n_agjx = return Invariant

instance Arbitrary TypeQualifier where
      arbitrary
        = sized go_agjy
        where
            go_agjy n_agjz
              = oneof
                  [TypeQualSto <$> resize (max 0 (n_agjz - 1)) arbitrary--,
                   {-TypeQualLay <$> resize (max 0 (n_agjz - 1)) arbitrary
                   <*> resize (max 0 (n_agjz - 1)) arbitrary,
                   TypeQualInt <$> resize (max 0 (n_agjz - 1)) arbitrary
                   <*> resize (max 0 (n_agjz - 1)) arbitrary,
                   TypeQualInv <$> resize (max 0 (n_agjz - 1)) arbitrary
                   <*> resize (max 0 (n_agjz - 1)) arbitrary,
                   TypeQualInv3 <$> resize (max 0 (n_agjz - 1)) arbitrary
                   <*> resize (max 0 (n_agjz - 1)) arbitrary
                   <*> resize (max 0 (n_agjz - 1)) arbitrary-}]

instance Arbitrary StructDeclarator where
      arbitrary
        = sized go_agjA
        where
            go_agjA n_agjB
              = StructDeclarator <$> resize (max 0 (n_agjB - 1)) arbitrary
                <*> resize (max 0 (n_agjB - 1)) arbitrary

instance Arbitrary Field where
      arbitrary
        = sized go_agjC
        where
            go_agjC n_agjD
              = Field <$> resize (max 0 (n_agjD - 1)) arbitrary
                <*> resize (max 0 (n_agjD - 1)) arbitrary
                <*> (listOf $ (resize (n_agjD `div` 10) arbitrary))

instance Arbitrary TypeSpecifierNonArray where
      arbitrary
        = sized go_agjE
        where
            go_agjE n_agjF
              = oneof
                  [return Void, return Float, return Int, return UInt, return Bool,
                   return Vec2, return Vec3, return Vec4, return BVec2, return BVec3,
                   return BVec4, return IVec2, return IVec3, return IVec4,
                   return UVec2, return UVec3, return UVec4, return Mat2, return Mat3,
                   return Mat4, return Mat2x2, return Mat2x3, return Mat2x4,
                   return Mat3x2, return Mat3x3, return Mat3x4, return Mat4x2,
                   return Mat4x3, return Mat4x4, return Sampler1D, return Sampler2D,
                   return Sampler3D, return SamplerCube, return Sampler1DShadow,
                   return Sampler2DShadow, return SamplerCubeShadow,
                   return Sampler1DArray, return Sampler2DArray,
                   return Sampler1DArrayShadow, return Sampler2DArrayShadow,
                   return ISampler1D, return ISampler2D, return ISampler3D,
                   return ISamplerCube, return ISampler1DArray,
                   return ISampler2DArray, return USampler1D, return USampler2D,
                   return USampler3D, return USamplerCube, return USampler1DArray,
                   return USampler2DArray, return Sampler2DRect,
                   return Sampler2DRectShadow, return ISampler2DRect,
                   return USampler2DRect, return SamplerBuffer, return ISamplerBuffer,
                   return USamplerBuffer, return Sampler2DMS, return ISampler2DMS,
                   return USampler2DMS, return Sampler2DMSArray,
                   return ISampler2DMSArray, return USampler2DMSArray,
                   StructSpecifier <$> resize (max 0 (n_agjF - 1)) arbitrary
                   <*> (listOf $ (resize (n_agjF `div` 10) arbitrary)),
                   TypeName <$> resize (max 0 (n_agjF - 1)) arbitrary]

instance Arbitrary TypeSpecifierNoPrecision where
      arbitrary
        = sized go_agjG
        where
            go_agjG n_agjH
              = TypeSpecNoPrecision <$> resize (max 0 (n_agjH - 1)) arbitrary
                <*> resize (max 0 (n_agjH - 1)) arbitrary

instance Arbitrary TypeSpecifier where
      arbitrary
        = sized go_agjI
        where
            go_agjI n_agjJ
              = TypeSpec <$> resize (max 0 (n_agjJ - 1)) arbitrary
                <*> resize (max 0 (n_agjJ - 1)) arbitrary

instance Arbitrary FunctionIdentifier where
      arbitrary
        = sized go_agjK
        where
            go_agjK n_agjL
              = oneof
                  [FuncIdTypeSpec <$> resize (max 0 (n_agjL - 1)) arbitrary,
                   FuncId <$> resize (max 0 (n_agjL - 1)) arbitrary]

instance Arbitrary Parameters where
      arbitrary
        = sized go_agjM
        where
            go_agjM n_agjN
              = oneof
                  [return ParamVoid,
                   Params <$> (listOf $ (resize (n_agjN `div` 10) arbitrary))]

instance Arbitrary Expr where
      arbitrary
        = sized go_agjO
        where
            go_agjO n_agjP
              = if (n_agjP <= 1) then
                    oneof
                      [Variable <$> resize (max 0 (n_agjP - 1)) arbitrary,
                       IntConstant <$> resize (max 0 (n_agjP - 1)) arbitrary
                       <*> resize (max 0 (n_agjP - 1)) arbitrary,
                       FloatConstant <$> resize (max 0 (n_agjP - 1)) arbitrary,
                       BoolConstant <$> resize (max 0 (n_agjP - 1)) arbitrary,
                       FunctionCall <$> resize (max 0 (n_agjP - 1)) arbitrary
                       <*> resize (max 0 (n_agjP - 1)) arbitrary]
                else
                    oneof
                      [Variable <$> resize (max 0 (n_agjP - 1)) arbitrary,
                       IntConstant <$> resize (max 0 (n_agjP - 1)) arbitrary
                       <*> resize (max 0 (n_agjP - 1)) arbitrary,
                       FloatConstant <$> resize (max 0 (n_agjP - 1)) arbitrary,
                       BoolConstant <$> resize (max 0 (n_agjP - 1)) arbitrary,
                       Bracket <$> (go_agjO $ (n_agjP `div` 2))
                       <*> (go_agjO $ (n_agjP `div` 2)),
                       FieldSelection <$> (go_agjO $ (n_agjP - 1))
                       <*> resize (max 0 (n_agjP - 1)) arbitrary,
                       MethodCall <$> (go_agjO $ (n_agjP - 1))
                       <*> resize (max 0 (n_agjP - 1)) arbitrary
                       <*> resize (max 0 (n_agjP - 1)) arbitrary,
                       FunctionCall <$> resize (max 0 (n_agjP - 1)) arbitrary
                       <*> resize (max 0 (n_agjP - 1)) arbitrary,
                       PostInc <$> (go_agjO $ (n_agjP - 1)),
                       PostDec <$> (go_agjO $ (n_agjP - 1)),
                       PreInc <$> (go_agjO $ (n_agjP - 1)),
                       PreDec <$> (go_agjO $ (n_agjP - 1)),
                       UnaryPlus <$> (go_agjO $ (n_agjP - 1)),
                       UnaryNegate <$> (go_agjO $ (n_agjP - 1)),
                       UnaryNot <$> (go_agjO $ (n_agjP - 1)),
                       UnaryOneComplement <$> (go_agjO $ (n_agjP - 1)),
                       Mul <$> (go_agjO $ (n_agjP `div` 2))
                       <*> (go_agjO $ (n_agjP `div` 2)),
                       Div <$> (go_agjO $ (n_agjP `div` 2))
                       <*> (go_agjO $ (n_agjP `div` 2)),
                       Mod <$> (go_agjO $ (n_agjP `div` 2))
                       <*> (go_agjO $ (n_agjP `div` 2)),
                       Add <$> (go_agjO $ (n_agjP `div` 2))
                       <*> (go_agjO $ (n_agjP `div` 2)),
                       Sub <$> (go_agjO $ (n_agjP `div` 2))
                       <*> (go_agjO $ (n_agjP `div` 2)),
                       LeftShift <$> (go_agjO $ (n_agjP `div` 2))
                       <*> (go_agjO $ (n_agjP `div` 2)),
                       RightShift <$> (go_agjO $ (n_agjP `div` 2))
                       <*> (go_agjO $ (n_agjP `div` 2)),
                       Lt <$> (go_agjO $ (n_agjP `div` 2))
                       <*> (go_agjO $ (n_agjP `div` 2)),
                       Gt <$> (go_agjO $ (n_agjP `div` 2))
                       <*> (go_agjO $ (n_agjP `div` 2)),
                       Lte <$> (go_agjO $ (n_agjP `div` 2))
                       <*> (go_agjO $ (n_agjP `div` 2)),
                       Gte <$> (go_agjO $ (n_agjP `div` 2))
                       <*> (go_agjO $ (n_agjP `div` 2)),
                       Equ <$> (go_agjO $ (n_agjP `div` 2))
                       <*> (go_agjO $ (n_agjP `div` 2)),
                       Neq <$> (go_agjO $ (n_agjP `div` 2))
                       <*> (go_agjO $ (n_agjP `div` 2)),
                       BitAnd <$> (go_agjO $ (n_agjP `div` 2))
                       <*> (go_agjO $ (n_agjP `div` 2)),
                       BitXor <$> (go_agjO $ (n_agjP `div` 2))
                       <*> (go_agjO $ (n_agjP `div` 2)),
                       BitOr <$> (go_agjO $ (n_agjP `div` 2))
                       <*> (go_agjO $ (n_agjP `div` 2)),
                       And <$> (go_agjO $ (n_agjP `div` 2))
                       <*> (go_agjO $ (n_agjP `div` 2)),
                       Or <$> (go_agjO $ (n_agjP `div` 2))
                       <*> (go_agjO $ (n_agjP `div` 2)),
                       Selection <$> (go_agjO $ (n_agjP `div` 3))
                       <*> (go_agjO $ (n_agjP `div` 3))
                       <*> (go_agjO $ (n_agjP `div` 3)),
                       Equal <$> (go_agjO $ (n_agjP `div` 2))
                       <*> (go_agjO $ (n_agjP `div` 2)),
                       MulAssign <$> (go_agjO $ (n_agjP `div` 2))
                       <*> (go_agjO $ (n_agjP `div` 2)),
                       DivAssign <$> (go_agjO $ (n_agjP `div` 2))
                       <*> (go_agjO $ (n_agjP `div` 2)),
                       ModAssign <$> (go_agjO $ (n_agjP `div` 2))
                       <*> (go_agjO $ (n_agjP `div` 2)),
                       AddAssign <$> (go_agjO $ (n_agjP `div` 2))
                       <*> (go_agjO $ (n_agjP `div` 2)),
                       SubAssign <$> (go_agjO $ (n_agjP `div` 2))
                       <*> (go_agjO $ (n_agjP `div` 2)),
                       LeftAssign <$> (go_agjO $ (n_agjP `div` 2))
                       <*> (go_agjO $ (n_agjP `div` 2)),
                       RightAssign <$> (go_agjO $ (n_agjP `div` 2))
                       <*> (go_agjO $ (n_agjP `div` 2)),
                       AndAssign <$> (go_agjO $ (n_agjP `div` 2))
                       <*> (go_agjO $ (n_agjP `div` 2)),
                       XorAssign <$> (go_agjO $ (n_agjP `div` 2))
                       <*> (go_agjO $ (n_agjP `div` 2)),
                       OrAssign <$> (go_agjO $ (n_agjP `div` 2))
                       <*> (go_agjO $ (n_agjP `div` 2)),
                       Sequence <$> (go_agjO $ (n_agjP `div` 2))
                       <*> (go_agjO $ (n_agjP `div` 2))]

instance Arbitrary CaseLabel where
      arbitrary
        = sized go_agjQ
        where
            go_agjQ n_agjR
              = oneof
                  [Case <$> resize (max 0 (n_agjR - 1)) arbitrary, return Default]

instance Arbitrary FullType where
      arbitrary
        = sized go_agjS
        where
            go_agjS n_agjT
              = FullType <$> resize (max 0 (n_agjT - 1)) arbitrary
                <*> resize (max 0 (n_agjT - 1)) arbitrary

instance Arbitrary InvariantOrType where
      arbitrary
        = sized go_agjU
        where
            go_agjU n_agjV
              = oneof
                  [--return InvariantDeclarator,
                   TypeDeclarator <$> resize (max 0 (n_agjV - 1)) arbitrary]

instance Arbitrary InitDeclarator where
      arbitrary
        = sized go_agjW
        where
            go_agjW n_agjX
              = InitDecl <$> resize (max 0 (n_agjX - 1)) arbitrary
                <*> resize (max 0 (n_agjX - 1)) arbitrary
                <*> resize (max 0 (n_agjX - 1)) arbitrary

instance Arbitrary Declaration where
      arbitrary
        = sized go_agjY
        where
            go_agjY n_agjZ
              = oneof
                  [InitDeclaration <$> resize (max 0 (n_agjZ - 1)) arbitrary
                   <*> (listOf $ (resize (n_agjZ `div` 10) arbitrary)),
                   --Precision <$> resize (max 0 (n_agjZ - 1)) arbitrary
                   -- <*> resize (max 0 (n_agjZ - 1)) arbitrary,
                   Block <$> resize (max 0 (n_agjZ - 1)) arbitrary
                    <*> resize (max 0 (n_agjZ - 1)) arbitrary
                    <*> (listOf $ (resize (n_agjZ `div` 10) arbitrary))
                    <*> resize (max 0 (n_agjZ - 1)) arbitrary] --,
                   --TQ <$> resize (max 0 (n_agjZ - 1)) arbitrary]

instance Arbitrary Condition where
      arbitrary
        = sized go_agk0
        where
            go_agk0 n_agk1
              = oneof
                  [Condition <$> resize (max 0 (n_agk1 - 1)) arbitrary,
                   InitializedCondition <$> resize (max 0 (n_agk1 - 1)) arbitrary
                   <*> resize (max 0 (n_agk1 - 1)) arbitrary
                   <*> resize (max 0 (n_agk1 - 1)) arbitrary]

instance Arbitrary Statement where
      arbitrary
        = sized go_agk2
        where
            go_agk2 n_agk3
              = if (n_agk3 <= 1) then
                    oneof
                      [DeclarationStatement <$> resize (max 0 (n_agk3 - 1)) arbitrary,
                       return Continue, return Break,
                       Return <$> resize (max 0 (n_agk3 - 1)) arbitrary,
                       return Language.GLSL.Syntax.Discard,
                       CompoundStatement <$> resize (max 0 (n_agk3 - 1)) arbitrary,
                       ExpressionStatement <$> resize (max 0 (n_agk3 - 1)) arbitrary,
                       CaseLabel <$> resize (max 0 (n_agk3 - 1)) arbitrary]
                else
                    oneof
                      [DeclarationStatement <$> resize (max 0 (n_agk3 - 1)) arbitrary,
                       return Continue, return Break,
                       Return <$> resize (max 0 (n_agk3 - 1)) arbitrary,
                       return Language.GLSL.Syntax.Discard,
                       CompoundStatement <$> resize (max 0 (n_agk3 - 1)) arbitrary,
                       ExpressionStatement <$> resize (max 0 (n_agk3 - 1)) arbitrary,
                       SelectionStatement <$> resize (max 0 (n_agk3 - 1)) arbitrary
                       <*> (go_agk2 $ (n_agk3 `div` 2))
                       <*> resize (max 0 (n_agk3 - 1)) arbitrary,
                       SwitchStatement <$> resize (max 0 (n_agk3 - 1)) arbitrary
                       <*> (listOf $ (resize (n_agk3 `div` 10) arbitrary)),
                       CaseLabel <$> resize (max 0 (n_agk3 - 1)) arbitrary,
                       While <$> resize (max 0 (n_agk3 - 1)) arbitrary
                       <*> (go_agk2 $ (n_agk3 - 1)),
                       DoWhile <$> (go_agk2 $ (n_agk3 - 1))
                       <*> resize (max 0 (n_agk3 - 1)) arbitrary,
                       For <$> resize (max 0 (n_agk3 - 1)) arbitrary
                       <*> resize (max 0 (n_agk3 - 1)) arbitrary
                       <*> resize (max 0 (n_agk3 - 1)) arbitrary
                       <*> (go_agk2 $ (n_agk3 - 1))]

instance Arbitrary Compound where
      arbitrary
        = sized go_agk4
        where
            go_agk4 n_agk5
              = Compound <$> (listOf $ (resize (n_agk5 `div` 10) arbitrary))

instance Arbitrary ParameterTypeQualifier where
      arbitrary
        = sized go_agk6
        where
            go_agk6 n_agk7 = return ConstParameter

instance Arbitrary ParameterQualifier where
      arbitrary
        = sized go_agk8
        where
            go_agk8 n_agk9
              = oneof
                  [return InParameter, return OutParameter, return InOutParameter]

instance Arbitrary ParameterDeclaration where
      arbitrary
        = sized go_agka
        where
            go_agka n_agkb
              = ParameterDeclaration <$> resize (max 0 (n_agkb - 1)) arbitrary
                <*> resize (max 0 (n_agkb - 1)) arbitrary
                <*> resize (max 0 (n_agkb - 1)) arbitrary
                <*> resize (max 0 (n_agkb - 1)) arbitrary

instance Arbitrary FunctionPrototype where
      arbitrary
        = sized go_agkc
        where
            go_agkc n_agkd
              = FuncProt <$> resize (max 0 (n_agkd - 1)) arbitrary
                <*> resize (max 0 (n_agkd - 1)) arbitrary
                <*> (listOf $ (resize (n_agkd `div` 10) arbitrary))

instance Arbitrary ExternalDeclaration where
      arbitrary
        = sized go_agke
        where
            go_agke n_agkf
              = oneof
                  [--FunctionDeclaration <$> resize (max 0 (n_agkf - 1)) arbitrary,
                   --FunctionDefinition <$> resize (max 0 (n_agkf - 1)) arbitrary
                   -- <*> resize (max 0 (n_agkf - 1)) arbitrary,
                   Declaration <$> resize (max 0 (n_agkf - 1)) arbitrary]

instance Arbitrary TranslationUnit where
      arbitrary
        = sized go_agkg
        where
            go_agkg n_agkh
              = TranslationUnit
                <$> (listOf $ (resize (n_agkh `div` 10) arbitrary))



$(devFixLang ''String ['Variable] ['Equal] ''TranslationUnit)

instance Fixable String x where
   fix = return

mencode :: MGLSL -> LC8.ByteString
mencode x = LC8.pack $ render $ option (Just x)

