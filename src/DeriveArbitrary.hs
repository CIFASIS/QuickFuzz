{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module DeriveArbitrary where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Test.QuickCheck
import Control.Monad
import Control.Applicative
import Data.List

import qualified Data.Set as S
import qualified Data.Graph as G
import qualified Data.Map.Strict as M
import Control.Monad.Trans.State.Lazy
import qualified Control.Monad.Trans.Class as TC


--data Tree a = Leaf a
--            | Node (Tree a) a (Tree a)
--            deriving (Eq, Show)
{-
instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = (arbitrary :: Gen Int) >>= go
    where go n | n <= 1 = Leaf <$> arbitrary
               | otherwise =
                 Node <$> (go (n `div` 2)) <*> arbitrary <*> (go (n `div` 2))

data Expr a = Var a
            | Con Int
            | Add (Expr a) (Expr a)
            | Mul (Expr a) (Expr a)
            | Neg (Expr a)
            deriving (Show, Eq)

instance Arbitrary a => Arbitrary (Expr a) where
  arbitrary = sized go
    --(arbitrary :: Gen Int) >>= go
    where go n | n <= 1 = oneof [Var <$> arbitrary, Con <$> arbitrary]
               | otherwise =
                 oneof [ go 1, Add <$> (go (n `div` 2)) <*> (go (n `div` 2))
                       ,Mul <$> (go (n `div` 2)) <*> (go (n `div` 2))
                       ,Neg <$> (go (n - 1))]

data A .. = C1 B | C2 ... 

data B .. = B1 A | B2 ...

instance Arbitrary ... => Arbitrary (A ..) where
    arbitrary = sized go
        where go n | n <= 1 = oneof [C2 ....]
                   | otherwise = resize (n-1) $ oneof [B1 .....]  
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
nm (SimpleCon n _ _) = n
tt (SimpleCon _ _ n) = n

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

isPrim :: Info -> Bool
isPrim (PrimTyConI _ _ _ ) = True
isPrim _ = False

chooseExpQ :: Name -> Integer -> Type -> ExpQ
chooseExpQ t bf (AppT ListT ty) = appE ( varE (mkName "listOf")) (appE (appE (varE (mkName "resize")) ([| ($(varE (mkName "n")) `div` 10) |])) (varE 'arbitrary))
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
                                              

deriveArbitrary :: Name -> Q [Dec]
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
                               | otherwise = oneof ( ($(listE (mkList fcs))) ++ $(listE (mkList scons))) |]
   else
    [d| instance Arbitrary $(applyTo (conT t) ns) where
                   arbitrary = sized go --(arbitrary :: Gen Int) >>= go
                     where go n | n <= 1 = oneof $(listE (mkList fcs))
                                | otherwise = oneof ( ($(listE (mkList fcs)))++ $(listE (mkList scons))) |]


isVarT (VarT _) = True
isVarT _ = False

findLeafTypes :: Type -> [Type]
findLeafTypes (AppT ListT ty) = findLeafTypes ty
findLeafTypes (AppT (TupleT n) ty) = findLeafTypes ty
findLeafTypes (AppT (ConT _) ty) = findLeafTypes ty
findLeafTypes (AppT ty1 ty2) = findLeafTypes ty1 ++ findLeafTypes ty2
findLeafTypes ty = [ty]

isArbInsName :: Name -> Q Bool
isArbInsName n = (isInstance ''Arbitrary [(ConT n)]) >>= (return . not)

deriveArbitraryRec :: Name -> Q [Dec]
deriveArbitraryRec t = do
  d <- reify t
  case d of
       TyConI (DataD _ _ _ constructors _) -> do
          let innerTypes = nub $ concat [ findLeafTypes ty | (simpleConView t -> SimpleCon _ 0 tys) <- constructors, ty <- tys, not (isVarT ty) ]
          runIO $ print innerTypes
          decs <- fmap concat $ forM innerTypes $ \ty ->
            do 
               tincho <- reify $ headOf ty
               if (isPrim tincho) then return [] 
               else do 
                       q <- isInstance ''Arbitrary [ty]
                       if not q
                         then do runIO $ putStrLn ("recursively deriving Arbitrary instance for " ++ show (headOf ty))
                                 deriveArbitraryRec (headOf ty)
                         else return []
          d <- deriveArbitrary t
          return (decs ++ d)
       e -> do
            runIO $ print $ "+++++++++++" ++ show e
            return []

--prueba de concepto
type StQ s a = StateT s Q a
type Names = [Name]

member :: Name -> StQ (M.Map Name Names) Bool
member t = do
    mk <- get
    return $ M.member t mk

addDep :: Name -> Names -> StQ (M.Map Name Names) ()
addDep n ns = do
    mapp <- get
    let newmapp = M.insert n ns mapp
    put newmapp

getDeps :: Name -> StQ (M.Map Name Names) ()
getDeps t = do
  visited <- member t
  if visited then return ()
  else do
              tip <- TC.lift $ reify t
              case tip of
                TyConI (DataD _ _ _ constructors _) -> do
                      let innerTypes = nub $ concat [ findLeafTypes ty | (simpleConView t -> SimpleCon _ 0 tys) <- constructors, ty <- tys, not (isVarT ty) ]
                      TC.lift $ runIO $ print $ "InnerTypes de ::" ++ show t
                      TC.lift $ runIO $ print innerTypes
                      {-
                         goodInnerTypes <- TC.lift $ filterM (\t -> do
                               i <- reify $ headOf t
                               if (isPrim i) then return False else (isInstance ''Arbitrary [t]) >>= (return . not)) innerTypes 
                      -}
                      let hof = map headOf innerTypes
                      addDep t hof
                      mapM_ getDeps hof
                TyConI (TySynD _ _ t) -> getDeps $ headOf t
                d -> do
                    TC.lift $ runIO $ print $ "Different?" ++ show d
                    return ()

-- Prueba de concepto, esto debe ser muy lento.
showDeps :: Name -> Q [Dec]
showDeps t = do
        mapp <- execStateT (getDeps t) M.empty 
        let rs = M.foldrWithKey (\ k d ds -> (k,k,d) : ds) [] mapp
        let (graph, v2ter, f) = G.graphFromEdges rs
        let topsorted = reverse $ G.topSort graph
        let ts' = map (\p -> (let (n,_,_) = v2ter p in n)) topsorted
        ts'' <- filterM isArbInsName ts'
        runIO $ print $ "Deberiamos derivar en este roden? ---" ++ show ts''
        ts <- mapM deriveArbitrary ts''  -- Ya podemos ir haciendo esto, total esta ordenado
        return $ concat ts
