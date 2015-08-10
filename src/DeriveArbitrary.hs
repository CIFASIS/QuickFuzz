{-# LANGUAGE TemplateHaskell #-}
module DeriveArbitrary where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Test.QuickCheck
import Control.Monad
import Control.Applicative
import Data.List

data Tree a = Leaf a
            | Node (Tree a) a (Tree a)
            deriving (Eq, Show)
{-
instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = (arbitrary :: Gen Int) >>= go
    where go n | n <= 1 = Leaf <$> arbitrary
               | otherwise =
                 Node <$> (go (n `div` 2)) <*> arbitrary <*> (go (n `div` 2))
-}
data Expr a = Var a
            | Con Int
            | Add (Expr a) (Expr a)
            | Mul (Expr a) (Expr a)
            | Neg (Expr a)
            deriving (Show, Eq)
{-
instance Arbitrary a => Arbitrary (Expr a) where
  arbitrary = (arbitrary :: Gen Int) >>= go
    where go n | n <= 1 = oneof [Var <$> arbitrary, Con <$> arbitrary]
               | otherwise =
                 oneof [Add <$> (go (n `div` 2)) <*> (go (n `div` 2))
                       ,Mul <$> (go (n `div` 2)) <*> (go (n `div` 2))
                       ,Neg <$> (go (n - 1))]
-}

countCons :: Name -> Type -> Integer
countCons n ty =
  case ty of
    ForallT _ _ t  -> countCons n t
    AppT ty1 ty2   -> countCons n ty1 + countCons n ty2
    SigT t _       -> countCons n t
    ConT t         -> if n == t then 1 else 0
    _              -> 0

branchingFactor :: Name -> Con -> Integer
branchingFactor tyName (NormalC _ sts) =
  sum $ map (countCons tyName . snd) sts

varNames = map (('a':) . show) [0..]

paramNames :: [TyVarBndr] -> [Name]
paramNames = map f
  where f (PlainTV n) = n
        f (KindedTV n _) = n

applyTo :: TypeQ -> [TypeQ] -> TypeQ
applyTo c ns =
  foldl (\h pn -> appT h pn) c ns

fixAppl :: Exp -> Exp
fixAppl (UInfixE e1@(UInfixE {}) op e2) = UInfixE (fixAppl e1) op e2
fixAppl (UInfixE con op e) = UInfixE con (VarE '(<$>)) e

headOf :: Type -> Name
headOf (AppT ty1 ty2) = headOf ty1
headOf (SigT ty _) = headOf ty
headOf (ConT n) = n
headOf (VarT n) = n
                                          
chooseExpQ :: Name -> Con -> Type -> ExpQ
chooseExpQ t c ty | headOf ty /= t = varE 'arbitrary
chooseExpQ t c _ =
  case branchingFactor t c of
    0  -> varE 'arbitrary
    1  -> appE (varE (mkName "go")) [| ($(varE (mkName "n")) - 1) |]
    bf -> appE (varE (mkName "go")) [| ($(varE (mkName "n")) `div` bf) |]
                                          
deriveArbitrary t = do
  TyConI (DataD _ _ params constructors _) <- reify t
  let ns = map varT $ paramNames params
  let mkList xs = map (fmap fixAppl)
                [ foldl (\h ty -> uInfixE h (varE '(<*>)) (chooseExpQ t con ty)) (conE name) tys'
                | con@(NormalC name tys) <- xs, let tys' = map snd tys  ]
  let (fcs,rcs) = partition ((==0) . branchingFactor t) constructors
  [d| instance $(applyTo (tupleT (length ns)) (map (appT (conT ''Arbitrary)) ns))
               => Arbitrary $(applyTo (conT t) ns) where
                 arbitrary = (arbitrary :: Gen Int) >>= go
                   where go n | n <= 1 = oneof $(listE (mkList fcs))
                              | otherwise = oneof $(listE (mkList rcs)) |]
