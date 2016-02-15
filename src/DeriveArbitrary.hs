{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module DeriveArbitrary where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Test.QuickCheck
import Control.Monad
import Control.Arrow
import Control.Applicative
import Data.List

--import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Graph as G
import qualified Data.Map.Strict as M
import Control.Monad.Trans.State.Lazy
import qualified Control.Monad.Trans.Class as TC

import Misc
import ByteString
--import Images
import Vector 

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

-- | Count 'how many times' a type is recursive to himself.
-- Used to make an arbitrary instance that reduce the size more accurate
countCons :: (Name -> Bool) -> Type -> Integer
countCons p ty =
  case ty of
    ForallT _ _ t  -> countCons p t
    AppT ty1 ty2   -> countCons p ty1 + countCons p ty2
    SigT t _       -> countCons p t
    ConT t         -> if p t then 1 else 0
    _              -> 0

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
                                          
-- | Look up  the first type name in a type structure.
headOf :: Type -> Name
headOf (AppT ListT ty) = headOf ty
headOf (AppT (TupleT _) ty) = headOf ty 
headOf (AppT ty1 _) = headOf ty1
headOf (SigT ty _) = headOf ty
headOf (ConT n) = n
headOf (VarT n) = n

-- | Check whether a type is a Primitive Type.
-- Something like Int#, Bool#, etc.
isPrim :: Info -> Bool
isPrim (PrimTyConI _ _ _ ) = True
isPrim _ = False


-- | Build the arbitrary function.
chooseExpQ :: Name -> Integer -> Type -> ExpQ
chooseExpQ t bf (AppT ListT ty) = appE ( varE (mkName "listOf")) (appE (appE (varE (mkName "resize")) ([| ($(varE (mkName "n")) `div` 10) |])) (varE 'arbitrary))
chooseExpQ t bf ty | headOf ty /= t = appE (appE (varE (mkName "resize")) ([|$(varE (mkName "n"))|])) (varE 'arbitrary)
chooseExpQ t bf ty =
  case bf of
    0  -> varE 'arbitrary
    1  -> appE (varE (mkName "go")) [| ($(varE (mkName "n")) - 1) |]
    bf -> appE (varE (mkName "go")) [| ($(varE (mkName "n")) `div` bf) |]

-- | View Pattern for Constructors
simpleConView :: Name -> Con -> ConView
simpleConView tyName c =
  let count = sum . map (countCons (== tyName))
  in case c of
  NormalC n sts -> let ts = map snd sts
                   in SimpleCon n (count ts) ts
  RecC n vsts   -> let ts = map proj3 vsts
                       proj3 (_,_,z) = z
                   in SimpleCon n (count ts) ts
  InfixC (_,t1) n (_,t2) ->
    SimpleCon n (countCons (== tyName) t1 + countCons (== tyName) t2) [t1,t2]
  ForallC _ _ innerCon -> simpleConView tyName innerCon
                                              

makeArbs t xs = map (fmap fixAppl) [ foldl (\h ty -> uInfixE h (varE '(<*>)) (chooseExpQ t bf ty)) (conE name) tys' | SimpleCon name bf tys' <- xs]

-- | Generic function used to create arbitrarily large tuples
-- do
--  a1 <- arbitrary
--  a2 <- arbitrary
--  ....
--  return $ (a1,a2,...)
genTupleArbs :: Int -> ExpQ
genTupleArbs n = 
    let ys = take n varNames
        xs = map mkName ys
         in
        doE $
             map (\x -> bindS (varP x) (varE 'arbitrary)) xs
            ++ [ noBindS $ appE (varE 'return) (tupE (map varE xs))]



-- | Get the first type in a type application.
-- Maybe we should improve this one
getTy :: Type -> Type
getTy (AppT t _) = getTy t
getTy t = t

-- | Give an arbitrary instance for its argument.
-- It doesn't check anything, just assume that it is ok to instance
-- its argument. And define the function arbitrary depending what type its
-- argument references to.
deriveArbitrary :: Name -> Q [Dec]
deriveArbitrary t = do
    inf <- reify t
    runIO $ print $ "Deriving:" ++ show inf
    case inf of
        TyConI (DataD _ _ params constructors _) -> do
              let ns  = map varT $ paramNames params
                  scons = map (simpleConView t) constructors
                  fcs = filter ((==0) . bf) scons
              if length ns > 0 then
               [d| instance $(applyTo (tupleT (length ns)) (map (appT (conT ''Arbitrary)) ns))
                            => Arbitrary $(applyTo (conT t) ns) where
                              arbitrary = sized go --(arbitrary :: Gen Int) >>= go
                                where go n | n <= 1 = oneof $(listE (makeArbs t fcs))
                                           | otherwise = oneof ( ($(listE (makeArbs t fcs))) ++ $(listE (makeArbs t scons))) |]
               else
                [d| instance Arbitrary $(applyTo (conT t) ns) where
                               arbitrary = sized go --(arbitrary :: Gen Int) >>= go
                                 where go n | n <= 1 = oneof $(listE (makeArbs t fcs))
                                            | otherwise = oneof ( ($(listE (makeArbs t fcs)))++ $(listE (makeArbs t scons))) |]
        TyConI (NewtypeD _ _ params con _) -> do
            let ns = map varT $ paramNames params
                scon = simpleConView t con
            if length ns > 0 then
               [d| instance $(applyTo (tupleT (length ns)) (map (appT (conT ''Arbitrary)) ns))
                            => Arbitrary $(applyTo (conT t) ns) where
                              arbitrary = sized go --(arbitrary :: Gen Int) >>= go
                                where go n | n <= 1 = oneof $(listE (makeArbs t [scon]))
                                           | otherwise = oneof ($(listE (makeArbs t [scon]))) |]
               else
                [d| instance Arbitrary $(applyTo (conT t) ns) where
                               arbitrary = sized go --(arbitrary :: Gen Int) >>= go
                                where go n | n <= 1 = oneof $(listE (makeArbs t [scon]))
                                           | otherwise = oneof ($(listE (makeArbs t [scon]))) |]
        TyConI (TySynD _ params ty) ->
            case (getTy ty) of
                (TupleT n) -> do
                        let ns = map varT $ paramNames params 
                        if (length ns > 0 ) then
                           [d| instance $(applyTo (tupleT (length ns)) (map (appT (conT ''Arbitrary)) ns))
                                        => Arbitrary $(applyTo (conT t) ns) where
                                          arbitrary = $(genTupleArbs n) |]
                        else -- Don't think we could ever enter here
                            fail "Tuple without arguments"
                (ConT n) -> return [] -- This type should had been derived already,
                                        -- It its a clearly dependency and it
                                        -- should be put before in the topsort.
                pepe -> do
                     runIO $ print "IGNORING"
                     runIO $ print ty
                     return []
        d -> do
          if (isPrim inf) then return [] else
            (fail $ "Case not defined: " ++ show d)

isVarT (VarT _) = True
isVarT _ = False

-- | Find all simple Types that are part of another Type.
findLeafTypes :: Type -> [Type]
findLeafTypes (AppT ListT ty) = findLeafTypes ty
findLeafTypes (AppT (TupleT n) ty) = findLeafTypes ty
findLeafTypes (AppT p@(ConT _) ty) = p : findLeafTypes ty
findLeafTypes (AppT ty1 ty2) = findLeafTypes ty1 ++ findLeafTypes ty2
findLeafTypes (VarT _) = []
findLeafTypes (ForallT _ _ ty) = findLeafTypes ty 
findLeafTypes ArrowT = []
findLeafTypes ListT = []
findLeafTypes StarT = []
findLeafTypes ty = [ty]


-- | Automatic recursive arbitrary derivation, DEPRECATED?
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

-- Be aware, down there is a world full of mysteries...
-- Impure computation is the least of your worries.
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

headOfNoVar :: Type -> [Name]
headOfNoVar (ConT n) = [n]
headOfNoVar (VarT _) = []
headOfNoVar (SigT t _ ) = headOfNoVar t
headOfNoVar (AppT ty1 ty2) = headOfNoVar ty1 ++ headOfNoVar ty2
headOfNoVar _ = []

getDeps :: Name -> StQ (M.Map Name Names) ()
getDeps t = do
  visited <- member t
  if (visited || hasArbIns t) then return ()
  else do
              TC.lift $ runIO $ print $ "PreVisiting:" ++ show t
              tip <- TC.lift $ reify t
              TC.lift $ runIO $ print $ "Visiting: " ++ show tip
              case tip of
                TyConI (DataD _ _ _ constructors _) -> do
                      let innerTypes = nub $ concat [ findLeafTypes ty | (simpleConView t -> SimpleCon _ _ tys) <- constructors, ty <- tys, not (isVarT ty) ]
                      let hof = map headOf innerTypes
                      addDep t hof
                      mapM_ getDeps hof
                TyConI (NewtypeD _ nm _ constructor _) -> do 
                      let (SimpleCon _ 0 ts )= simpleConView nm constructor
                      let innerTypes = nub $ concat $ map findLeafTypes $ filter (not . isVarT) ts
                      let hof = map headOf innerTypes
                      addDep t hof
                      mapM_ getDeps hof
                TyConI (TySynD _ _ m) -> do
                    addDep t (headOfNoVar m)
                    mapM_ getDeps (headOfNoVar m) 
                d -> 
                    if (isPrim tip) then return () else return ()


tocheck :: [TyVarBndr] -> Name -> Type
tocheck bndrs nm =
    let ns = map VarT $ paramNames bndrs in foldl (\r a -> AppT r a) (ConT nm) ns
    

hasArbIns :: Name -> Bool
hasArbIns n = isPrefixOf "GHC." (show n) || isPrefixOf "Data.Vector" (show n) || isPrefixOf "Data.Text" (show n) || isPrefixOf "Codec.Image" (show n) 


isArbInsName :: Name -> Q Bool
isArbInsName n = do
        inf <- reify n
        case inf of
            TyConI (DataD _ _ preq _ _) -> 
                        if length preq > 0 then
                                (isInstance ''Arbitrary [tocheck preq n]) >>= (return . not)
                        else
                                (isInstance ''Arbitrary [(ConT n)]) >>= (return . not)
            TyConI (NewtypeD _ _ preq _ _) -> 
                        if length preq > 0 then
                                (isInstance ''Arbitrary [tocheck preq n]) >>= (return . not)
                        else
                                (isInstance ''Arbitrary [(ConT n)]) >>= (return . not)
            TyConI (TySynD _ preq _ ) -> 
                        if length preq > 0 then
                                (isInstance ''Arbitrary [tocheck preq n]) >>= (return . not)
                        else
                                (isInstance ''Arbitrary [(ConT n)]) >>= (return . not)
            d -> do
                runIO $ print $ "Weird case:: " ++ show d
                isInstance ''Arbitrary [(ConT n)] >>= (return . not)

-- TODO: add debugging flag, or remove all those prints.
showDeps :: Name -> Q [Dec]
showDeps t = do
        mapp <- execStateT (getDeps t) M.empty 
        let rs = M.foldrWithKey (\ k d ds -> (k,k,d) : ds) [] mapp
        let (graph, v2ter, f) = G.graphFromEdges rs
        let topsorted = reverse $ G.topSort graph
        let ts' = map (\p -> (let (n,_,_) = v2ter p in n)) topsorted
        runIO $ print $ "Topologically sorted types" ++ show ts'
        ts'' <- filterM isArbInsName ts'
        --ts'' <- filterM isNotBasic ts''
        runIO $ print $ "We should derive in this order ---" ++ show ts''
        ts <- mapM (\t -> (runIO $ print $ show t) >> deriveArbitrary t) ts'' -- Notice here, we call
        -- deriveArbitrary directly, because we are fully confident we can
        -- derive all the types in that order.
        return $ concat ts

getNeedFun :: Type -> (Int,[Type])
getNeedFun (ForallT xs _ ty) = let ns = findLeafTypes ty in (length xs, ns)

-- | Define an arbitrary instance based on some function.
-- For example: Data A a = ....
-- f :: ... -> A a
-- g :: ... -> A a
-- ... etc
-- instance [Arbitrary a =>] Arbitrary (A a)  where
--      arbitrary = do
--              as <- repeatM arbitrary
--              oneof $ map pure [f ..., g ..., ....] 
arbFunBased :: Name -- ^ Data type name, 'A'
            -> [Name] -- ^ Function names, 'f' 'g' etc
            -> Q [Dec] -- ^ instance Arbitrary A...
arbFunBased n ns = do
        runIO $ print n
        nsty <- mapM (\x -> 
            do
                (VarI _ ty _  _) <- reify x
                return ty
            ) ns
        --let lfs = (nub $ (foldl (\ r x -> findLeafTypes x ++ r ) [] nsty)) \\ [ConT n]
        let (fv,lfs) = (foldl (\ (is,rs) x -> let (i,xs) =  getNeedFun x in (i+is,xs ++ rs)) (0,[]) nsty)
        let lfs' = (nub $ lfs) \\ [ConT n]
        runIO $ print (fv,lfs')
        return []
