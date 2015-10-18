{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module DeriveArbitrary where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Test.QuickCheck
import Control.Monad
import Control.Applicative
import Data.List

--data Tree a = Leaf a
--            | Node (Tree a) a (Tree a)
--            deriving (Eq, Show)
{-
instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = (arbitrary :: Gen Int) >>= go
    where go n | n <= 1 = Lyy
eaf <$> arbitrary
               | otherwise =
                 Node <$> (go (n `div` 2)) <*> arbitrary <*> (go (n `div` 2))

data Expr a = Var a
            | Con Int
            | Add (Expr a) (Expr a)
            | Mul (Expr a) (Expr a)
            | Neg (Expr a)
            deriving (Show, Eq)

instance Arbitrary a => Arbitrary (Expr a) where
  arbitrary = (arbitrary :: Gen Int) >>= go
    where go n | n <= 1 = oneof [Var <$> arbitrary, Con <$> arbitrary]
               | otherwise =
                 oneof [Add <$> (go (n `div` 2)) <*> (go (n `div` 2))
                       ,Mul <$> (go (n `div` 2)) <*> (go (n `div` 2))
                       ,Neg <$> (go (n - 1))]
-}

data Point = L | H deriving (Show, Eq)

data Expr2 a = Var2 a
             | Con2 Int
             | Tr [Point]
             | Add2 (Expr2 a) (Expr2 a)
             | Mul2 (Expr2 a) (Expr2 a)
             | Neg2 (Expr2 a)
             deriving (Show, Eq)


data ConView = SimpleCon Name Integer [Type]

bf (SimpleCon _ n _) = n

countCons :: (Name -> Bool) -> Type -> Integer
countCons p ty =
  case ty of
    ForallT _ _ t  -> countCons p t
    AppT ty1 ty2   -> countCons p ty1 + countCons p ty2
    SigT t _       -> countCons p t
    ConT t         -> if p t then 1 else 0
    _              -> 0
{-
branchingFactor :: Name -> Con -> Integer
branchingFactor tyName (NormalC _ sts) =
  sum $ map (countCons (== tyName) . snd) sts
branchingFactor tyName (RecC _ vsts) =
  sum $ map (countCons (== tyName) . proj3) vsts
    where proj3 (x,y,z) = z
branchingFactor tyName (InfixC (_,t1) _ (_,t2)) =
  countCons (== tyName) t1 +   countCons (== tyName) t2
branchingFactor tyName (ForallC _ _ c) = branchingFactor tyName c  
-}

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
fixAppl e = AppE (VarE 'return) e
                                          
headOf :: Type -> Name
headOf (AppT ListT ty) = headOf ty
headOf (AppT (TupleT n) ty) = headOf ty 
headOf (AppT ty1 ty2) = headOf ty1
headOf (SigT ty _) = headOf ty
headOf (ConT n) = n
headOf (VarT n) = n

chooseExpQ :: Name -> Integer -> Type -> ExpQ
chooseExpQ t bf ty | headOf ty /= t = varE 'arbitrary
chooseExpQ t bf ty =
  case bf of
    0  -> varE 'arbitrary
    1  -> appE (varE (mkName "go")) [| ($(varE (mkName "n")) - 1) |]
    bf -> appE (varE (mkName "go")) [| ($(varE (mkName "n")) `div` bf) |]

simpleConView :: Name -> Con -> ConView
simpleConView tyName c =
  let count = sum . map (countCons (== tyName))
  in case c of
  NormalC n sts -> let ts = map snd sts
                   in SimpleCon n (count ts) ts
  RecC n vsts   -> let ts = map proj3 vsts
                       proj3 (x,y,z) = z
                   in SimpleCon n (count ts) ts
  InfixC (_,t1) n (_,t2) ->
    SimpleCon n (countCons (== tyName) t1 + countCons (== tyName) t2) [t1,t2]
  ForallC _ _ innerCon -> simpleConView tyName innerCon
                                              

deriveArbitrary t = do
  TyConI (DataD _ _ params constructors _) <- reify t
  let mkList xs = map (fmap fixAppl)
                  [ foldl (\h ty -> uInfixE h (varE '(<*>)) (chooseExpQ t bf ty)) (conE name) tys' | SimpleCon name bf tys' <- xs]
  let ns  = map varT $ paramNames params
      scons = map (simpleConView t) constructors
      fcs = filter ((==0) . bf) scons
  if length ns > 0 then
   [d| instance $(applyTo (tupleT (length ns)) (map (appT (conT ''Arbitrary)) ns))
                => Arbitrary $(applyTo (conT t) ns) where
                  arbitrary = sized go --(arbitrary :: Gen Int) >>= go
                    where go n | n <= 1 = oneof $(listE (mkList fcs))
                               | otherwise = oneof $(listE (mkList scons)) |]
   else
    [d| instance Arbitrary $(applyTo (conT t) ns) where
                   arbitrary = sized go --(arbitrary :: Gen Int) >>= go
                     where go n | n <= 1 = oneof $(listE (mkList fcs))
                                | otherwise = oneof $(listE (mkList scons)) |]


isVarT (VarT _) = True
isVarT _ = False

findLeafTypes :: Type -> [Type]
findLeafTypes (AppT ListT ty) = findLeafTypes ty
findLeafTypes (AppT (TupleT n) ty) = findLeafTypes ty
findLeafTypes (AppT (ConT _) ty) = findLeafTypes ty
findLeafTypes (AppT ty1 ty2) = findLeafTypes ty1 ++ findLeafTypes ty2
findLeafTypes ty = [ty]

deriveArbitraryRec t = do
  TyConI (DataD _ _ _ constructors _) <- reify t
  let innerTypes = nub $ concat [ findLeafTypes ty | (simpleConView t -> SimpleCon _ 0 tys) <- constructors, ty <- tys, not (isVarT ty) ]
  runIO $ print innerTypes
  decs <- fmap concat $ forM innerTypes $ \ty ->
    do q <- isInstance ''Arbitrary [ty]
       if not q
         then do runIO $ putStrLn ("recursively deriving Arbitrary instance for " ++ show (headOf ty))
                 deriveArbitraryRec (headOf ty)
         else return []
  d <- deriveArbitrary t
  return (decs ++ d)
