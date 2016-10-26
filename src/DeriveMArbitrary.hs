{-# LANGUAGE TemplateHaskell #-}

module DeriveMArbitrary where

import Data.List.Split
import Data.List
import Data.Maybe

import Control.Monad
import Control.Applicative
import Control.Exception

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Language.Haskell.GhcMod
import Language.Haskell.GhcMod.Browse

import Language.Haskell.Meta.Parse

import Test.QuickCheck

import Megadeth.Prim

import Debug.Trace

-------------------------------------------------
--             Module crawler                  --
-------------------------------------------------

data Declaration = D { fid   :: Name     -- Unique identifier used inside the actions adt
                     , fname :: Name     -- Name of the function to be called by perform 
                     , oper  :: Bool     -- The function is an infix operator? (e.g.  (!))
                     , utv   :: [Name]   -- Universally quantified type vars
                     , ctv   :: [Type]   -- Constrained type vars (e.g.  (AppT (ConT Maybe) (VarT a))
                     , ty    :: [Type]   -- Type signature of each function parameter (splitted by "->")
                     }

instance Show Declaration where
    show d = show (fid d, fname d, oper d, utv d, ctv d, ty d)


opts = BrowseOpts { optBrowseOperators = True
                  , optBrowseDetailed  = True
                  , optBrowseQualified = True }



getModuleExports :: String -> IO ([Declaration], [String])
getModuleExports mod = do
    (res, log) <- runGhcModT defaultOptions $ browse opts mod
    case res of
        Left err  -> error $ "error browsing module: " ++ mod
        Right str -> return (ok, failed)
            where (ok, failed, _) = foldr parse ([],[],0) $ lines str


parse :: String -> ([Declaration], [String], Int) -> ([Declaration], [String], Int)
parse str (ok, failed, n) = 
    let parsedDecl = do
            (name, sig) <- splitDec str
            (full, isOp) <- parseName name
            ty <- parseSignature sig
            return (full, isOp, ty)
    in case parsedDecl of
        Nothing -> (ok, str:failed, n)
        Just (full, isOp, ty) -> (dec:ok, failed, n')
            where dec = D {fid=dfid, fname=full, oper=isOp, utv=dutv, ctv=dctv, ty=dty}
                  (dutv, dctv, dty) = flattenType ty
                  (dfid, n') | isOp    = (mkOpName n, n+1)
                             | otherwise = (unqualify full, n)


-- | Split declaration into (name, type)
splitDec :: String -> Maybe (String, String)
splitDec str = case splitOn "::" str of
    [name, sig] -> Just (name, sig)
    _ -> Nothing

-- | Parse function name and check if is an infix operator
parseName :: String -> Maybe (Name, Bool)
parseName str = case parseExp str of
    Right (VarE n) -> Just (n, False)
    Right (ConE n) -> Just (n, False)
    Right (UInfixE (ConE m) _ (VarE op)) -> Just  (mkName $ show m ++ "." ++ show op ++ "", True)
    _ -> Nothing 

-- | Parse funtion signature 
parseSignature :: String -> Maybe Type
parseSignature str = 
    let str' = "dummy :: " ++ filter (/='#') str
    in case parseDecs str' of
        Right [SigD _ ty] -> Just ty
        _ -> Nothing


-- | Transform type ast into (utv, ctv, ty)
flattenType :: Type -> ([Name], [Type], [Type])
flattenType t = (dutv \\ constTVars dctv, dctv, dty)
    where (dutv, dctv, dty) = flattenType' t    

flattenType' (ForallT tvb cxt ty) = 
    let (tvb', cxt', ty') = flattenType' ty
    in (map fromTyVarBndr tvb ++ tvb', cxt ++ cxt', ty')
flattenType' (AppT (AppT ArrowT l) r) = 
    let (tvb', cxt', r') = flattenType' r
    in (tvb', cxt', l:r')
flattenType' t = ([], [], [t])


-- | Extract type vars constrained in a list of type constraints 
constTVars :: [Type] -> [Name]
constTVars = rmdups . concatMap const
    where const (AppT l r) = const l ++ const r
          const (VarT n) = [n]
          const _ = []

-- | Flatten TyVarBndr into Name
fromTyVarBndr :: TyVarBndr -> Name
fromTyVarBndr (PlainTV n) = n
fromTyVarBndr (KindedTV n _) = n

-- foldr (\(dec, n) decs -> mkSubName dec n : decs)



-------------------------------------------------
--  actions tree and perform code generation   --
-------------------------------------------------
devActions :: [String] -> Name -> Bool -> [Name] -> Q [Dec]
devActions mods tname monad tinst = do
    modexps <- runIO $ mapM getModuleExports mods       -- [([Declaration], [String])]
    let indexed = zip (map fst modexps) [1..]           -- [([Declaration], Int)]
        addModIndex (mod,n) = map (\dec-> dec {fid = mkSubName (fid dec) n}) mod
        decs = concat $ map addModIndex indexed         -- [Declaration]
        cons = getConstraints decs
    instances <- getInstances cons tinst
    let decs' = concatMap (instantiateConstraints instances) decs
        decs'' = concatMap (instantiateTVars tinst) decs'
        actions = filter (\d -> resultType tname d && decidableArgs d) decs'' 
    adt <- devADT monad tname actions
    run <- devPerform monad tname actions
    return [adt, run]






devADT :: Bool -> Name -> [Declaration] -> DecQ
devADT isMonad tname funcs = 
    dataD (cxt []) (mkTypeName tname) [] (map (devCon isMonad tname) funcs) [] 
  
devCon :: Bool -> Name -> Declaration -> ConQ
devCon isMonad tname dec = 
    normalC (mkConName tname (fid dec)) (map (devBangType isMonad tname) (init (ty dec)))

devBangType :: Bool -> Name -> Type -> StrictTypeQ
devBangType isMonad tname t = strictType notStrict replaceArgTy 
    where replaceArgTy | tname `compat` t  = thisType 
                       | otherwise         = return t
          thisType | isMonad    = appT listT (conT $ mkTypeName tname)
                   | otherwise  = conT $ mkTypeName tname
                                      


devPerform :: Bool -> Name -> [Declaration] -> DecQ
devPerform isMonad tname actions = funD (mkPerformName tname) clauses
    where clauses | isMonad   = clausesM
                  | otherwise = clausesP
          clausesM = emptyClause : (map (devClauseM tname) actions)
          clausesP = map (devClauseP tname) actions
          emptyClause = clause [listP []] (normalB [| return () |]) []

devClauseM :: Name -> Declaration -> ClauseQ
devClauseM tname dec = clause [consMatch] consBody []
    where cname = mkConName tname (fid dec)
          nargs = length (ty dec) - 1
          varsP = map (varP . mkVarName tname) [1..nargs]  
          varsE = map (varE . mkVarName tname) [1..nargs]
          actionP = conP cname varsP
          consMatch = infixP actionP (mkName ":") (varP $ mkVarMore tname)      
          consBody = (normalB (uInfixE (apply tname (dec {ty = init (ty dec)}) varsE) 
                              (varE $ mkName ">>") 
                              (appE (varE $ mkPerformName tname) (varE $ mkVarMore tname))))

devClauseP :: Name -> Declaration -> ClauseQ
devClauseP tname dec = clause [conP cname varsP] (normalB (apply tname (dec {ty = init (ty dec)}) varsE)) []
    where cname = mkConName tname (fid dec)
          nargs = length (ty dec) - 1
          varsP = map (varP . mkVarName tname) [1..nargs]  
          varsE = map (varE . mkVarName tname) [1..nargs]

apply :: Name -> Declaration -> [ExpQ] -> ExpQ
apply tname dec [] = funE (fname dec) 
apply tname dec vs = appE (apply tname (dec {ty = init (ty dec)}) (init vs)) replaceAction 
    where replaceAction | tname `compat` last (ty dec) = appE (varE (mkPerformName tname)) (last vs)
                        | otherwise                    = last vs

funE n = case parseExp $ nameBase n of
    Right (ConE _) -> conE n 
    _ -> varE n



-- | Return all valid instances from a list of type names
-- and a list of class names [(classname, typename)]
getInstances :: [Name] -> [Name] -> Q [(Name,Name)]
getInstances classes types = filterM instanceOf tuples
    where tuples = liftA2 (,) classes types 
          instanceOf (c, t) = do              
                compat <- compatKinds c t -- This is still <very> experimental
                if compat                       
                    then not <$> isinsName c t  
                    else return False           


-- | Instantiate a declaration with non-empty universaly
-- quantified type vars with all permutations of a given
-- list of type names, renaming each one
instantiateTVars :: [Name] -> Declaration -> [Declaration]
instantiateTVars types dec@(D{utv=[]}) = [dec] 
instantiateTVars types dec = map mkNewDec tuples
    where tperms = replicateM (length (utv dec)) (map ConT types)
          dnames = zipWith mkSubName (repeat (fid dec)) [1..]
          tuples = zip dnames tperms
          mkNewDec (dname, tperm) = dec {fid=dname, utv=[], ty=replaceList (utv dec) tperm (ty dec)}


-- | Instantiate a declaration with non-empty type constraints
-- using a list of tuples (classname, typename) of valid instances
instantiateConstraints :: [(Name, Name)] -> Declaration -> [Declaration]
instantiateConstraints [] dec = [dec]
instantiateConstraints _  dec@(D{ctv=[]}) = [dec]
instantiateConstraints instances dec = map mkNewDec namedBinds 
    where dnames = zipWith mkSubName (repeat (fid dec)) [1..]
          dcons = map splitConstraint (ctv dec)   -- [(classname,varname)]
          binds = bindInstances instances dcons
          validperms = foldr (\binds rem -> unzip binds : rem) [] binds 
          namedBinds = zipWith (\dname (vs, ts) -> (dname, vs, ts)) dnames validperms
          mkNewDec (dname, vs, tperm) = dec {fid=dname, ty=replaceList vs tperm (ty dec)}

-- | Replace a list of var with a list of types in a declaration types list
replaceList :: [Name] -> [Type] -> [Type] -> [Type]
replaceList [] [] ty = ty
replaceList (v:vs) (tp:tps) ty = replaceList vs tps (map (replaceTVar v tp) ty)
replaceList v tp _ = error $ "replaceTVar: unexpected input: " ++ show v ++", " ++ show tp


-- | Bind a list of instances with a list of tuples (classname,varname)
-- returning a list of all permutations which satisfies class constraints
bindInstances :: [(Name,Name)] -> [(Name,Name)] -> [[(Name,Type)]]
bindInstances instances [] = []
bindInstances instances ((c,v):tcs) = sequence (binds :  bindInstances instances tcs) 
    where validTypes = filter ((==c) . fst) instances
          binds = map ((,) v . ConT . snd) validTypes          


-- | Replace VarT=v with Type=t in a given type
replaceTVar :: Name -> Type -> Type -> Type
replaceTVar v t (AppT l r) = AppT (replaceTVar v t l) (replaceTVar v t r)
replaceTVar v t (VarT n) | v == n    = t
                         | otherwise = VarT n
replaceTVar _ _ t = t



-- | Extract the kind asociated to a type name
getKind :: Name -> TypeQ
getKind n = do
    info <- reify n
    case info of
        ClassI (ClassD _ _ tvb _ _) _ -> makeKind tvb 
        TyConI (DataD _ _ tvb _ _) -> makeKind tvb
        TyConI (NewtypeD _ _ tvb _ _) -> makeKind tvb
        TyConI (TySynD _ tvb _) -> makeKind tvb        -- This doesn't look to work properly
        t -> error $ "getKind: unexpected reify info: " ++ show t 

makeKind :: [TyVarBndr] -> TypeQ
makeKind tvb = return $ foldr (\l r -> AppT (AppT ArrowT l) r) StarT $ map toKind tvb

toKind :: TyVarBndr -> Type
toKind (PlainTV _) = StarT
toKind (KindedTV _ k) = k

compatKinds :: Name -> Name -> Q Bool
compatKinds cname tname = do
    ckind <- getKind cname
    tkind <- getKind tname
    case ckind of
        AppT (AppT ArrowT t) StarT -> return $ t == tkind
        _ -> return False


-- | Convert type constraint into (Class, TVar)
-- NOTE: Only one type var supported
splitConstraint :: Type -> (Name, Name)
splitConstraint (AppT (ConT c) (VarT v)) = (c,v)
splitConstraint t = error $ "splitConstraint: unexpected input: " ++ show t


-- | The given declaration contains type vars?
decidableArgs :: Declaration -> Bool
decidableArgs dec = all (not . hasVarT) (ty dec)

hasVarT :: Type -> Bool
hasVarT (VarT _) = True
hasVarT (AppT l r) = hasVarT l || hasVarT r
hasVarT (ConT _) = False
hasVarT _ = False


-- Compare type name and the result type of a declaration
resultType :: Name -> Declaration -> Bool
resultType tname dec = tname `compat` last (ty dec)

compat :: Name -> Type -> Bool
compat tname (AppT l r) = compat tname l
compat tname (ConT c) = unqualify tname == unqualify c
compat _ _ = False


-- | Extract all type constraint names from a declaration batch
getConstraints :: [Declaration] -> [Name]
getConstraints = rmdups . map (fst . splitConstraint) . concatMap ctv

-- | Create Arbitrary instance for the original type
-- trivially using its derived actions type
devArbitraryWithActions :: Bool -> Name -> DecsQ
devArbitraryWithActions isMonad tname =
    let sig | isMonad   = appT listT (conT $ mkTypeName tname)
            | otherwise = conT $ mkTypeName tname
        perform = varE $ mkPerformName tname
    in [d| instance Arbitrary $(conT tname) where
                arbitrary = (arbitrary :: Gen $sig) >>= return . $perform 
       |]


-------------------------------------------------
--                  Helpers                    --
-------------------------------------------------

-- | Name manipulations
mkTypeName tname = mkName $ nameBase tname ++ "Action"
mkPerformName tname = mkName $ "perform" ++ nameBase tname
mkSubName f n = mkName $ nameBase f ++ "_" ++ show n
mkOpName n = mkName $ "infix" ++ show n
mkConName tname f = mkName $ "Act_" ++ nameBase tname ++ "_" ++ nameBase f
mkVarName tname v = mkName $ "v" ++ show v ++ "_" ++ nameBase tname
mkVarMore tname = mkName $ "more" ++ nameBase tname ++ "Actions"

unqualify :: Name -> Name
unqualify = mkName . nameBase


-- | Remove duplicates
rmdups :: (Ord a, Eq a) => [a] -> [a]
rmdups = map head . group . sort


-------------------------------------------------
--                   Tests                     --
-------------------------------------------------
testParser :: String -> Name -> [Name] -> IO ()
testParser mod tname types  = do 
    (decs, failed) <- getModuleExports mod
    putStrLn "--------------- RAW ------------------"
    mapM_ print decs 
    putStrLn "----------- INSTANTIATED  ------------"
    let decs' = concatMap (instantiateTVars types) decs 
    mapM_ print decs'
    putStrLn "------------- ACTIONS ----------------"
    let actions = filter (\d -> resultType tname d && decidableArgs d) decs' 
    mapM_ print actions
    putStrLn "------------ CONSTRAINTS -------------"
    mapM_ print $ getConstraints decs'
    putStrLn "-------------- FAILED ----------------"
    mapM_ putStrLn failed
