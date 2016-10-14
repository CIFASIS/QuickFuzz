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

import Megadeth.Prim

import Debug.Trace

-------------------------------------------------
--             Module crawler                  --
-------------------------------------------------
-- type Declaration = (Name, [Name], Cxt, [Type])


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
    (res, log) <- handle (\e -> error (show (e :: SomeException))) $ runGhcModT defaultOptions $ browse opts mod
    case res of
        Left err  -> error $ "error browsing module: " ++ mod
        Right str -> trace str $ return (ok, failed)
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
    Right (UInfixE (ConE m) _ (VarE op)) -> Just  (mkName $ show m ++ ".(" ++ show op ++ ")", True)
    _ -> Nothing 

-- | Parse funtion signature 
parseSignature :: String -> Maybe Type
parseSignature str = case parseDecs ("dummy :: " ++ str) of
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


unqualify :: Name -> Name
unqualify = mkName . nameBase



-------------------------------------------------
--  actions tree and perform code generation   --
-------------------------------------------------

devMArbitrary :: String -> Name -> Bool -> [Name] -> Q [Dec]
devMArbitrary mod tname monad tinst = do
    (decs, failed) <- runIO $ getModuleExports $ mod
    let cons = getConstraints decs
    instances <- getInstances cons tinst
    let decs' = concatMap (instantiate tinst) decs
        decs'' = concatMap (concretizeConstraints instances) decs'
        actions = filter (\d -> resultType tname d && decidableArgs d) decs' 
    adt <- devADT monad tname actions
    run <- devRun monad tname actions
    --runIO $ mapM_ print decs'' 
    runIO $ mapM_ print decs 
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
                                      


devRun :: Bool -> Name -> [Declaration] -> DecQ
devRun isMonad tname actions = funD (mkPerformName tname) clauses
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
apply tname dec [] = varE (fname dec) 
apply tname dec vs = appE (apply tname (dec {ty = init (ty dec)}) (init vs)) replaceAction 
    where replaceAction | tname `compat` last (ty dec) = appE (varE (mkPerformName tname)) (last vs)
                        | otherwise                    = last vs
          


mkTypeName tname = mkName $ nameBase tname ++ "Action"
mkOpName n = mkName $ "infix" ++ show n
mkConName tname f = mkName $ "Act_" ++ nameBase tname ++ "_" ++ nameBase f
mkVarName tname v = mkName $ "v" ++ show v ++ "_" ++ nameBase tname
mkPerformName tname = mkName $ "perform" ++ nameBase tname
mkVarMore tname = mkName $ "more" ++ nameBase tname ++ "Actions"
mkFunSubName f n = mkName $ nameBase f ++ "_" ++ show n



rmdups :: (Ord a, Eq a) => [a] -> [a]
rmdups = map head . group . sort


getConstraints :: [Declaration] -> [Name]
getConstraints = rmdups . map getClassName . concatMap ctv

getClassName :: Type -> Name
getClassName (ConT n) = n
getClassName (AppT l r) = getClassName l
getClassName _ = error "unexpected type"

getInstances :: [Name] -> [Name] -> Q [(Name,Name)]
getInstances types classes = filterM instanceOf tuples
    where tuples = liftA2 (,) types classes 
          instanceOf (t, c) = not <$> isinsName t c

resultType :: Name -> Declaration -> Bool
resultType tname dec = tname `compat` last (ty dec)

compat :: Name -> Type -> Bool
compat tname (AppT l r) = compat tname l
compat tname (ConT c) = unqualify tname == unqualify c  
compat _ _ = False

decidableArgs :: Declaration -> Bool
decidableArgs dec = all (not . hasVarT) (ty dec)

hasVarT :: Type -> Bool
hasVarT (VarT _) = True
hasVarT (AppT l r) = hasVarT l || hasVarT r
hasVarT (ConT _) = False
hasVarT _ = False


-- | Instantiate a declaration with non-empty universaly
-- quantified type vars with all permutations of a given
-- list of type names, renaming each one
instantiate :: [Name] -> Declaration -> [Declaration]
instantiate types dec@(D{utv=[]}) = [dec] 
instantiate types dec = map mkNewDec tuples
    where tperms = replicateM (length (utv dec)) (map ConT types)
          dnames = zipWith mkFunSubName (repeat (fid dec)) [1..]
          tuples = zip dnames tperms
          replaceList tp = liftM3 replaceTVar (utv dec) tp (ty dec) 
          mkNewDec (dname, tperm) = dec {fid=dname, utv=[], ty=replaceList tperm} 



-- | Instantiate a declaration with non-empty type constraints
-- using a list of tuples (Class, Type) of valid instances
concretizeConstraints :: [(Name, Name)] -> Declaration -> [Declaration]
concretizeConstraints [] dec = [dec]
concretizeConstraints _  dec@(D{ctv=[]}) = [dec]
concretizeConstraints instances dec = map mkNewDec namedBinds 
    where tc = catMaybes $ map typeToTuple $ ctv dec   -- [(classname,varname)]
          binds = auxFunc instances tc  
          validperms = foldr (\binds rem -> unzip binds : rem) [] binds 
          dnames = zipWith mkFunSubName (repeat (fid dec)) [1..]
          namedBinds = zipWith (\dname (vs, ts) -> (dname, vs, ts)) dnames validperms
          replaceList vs tperm = liftM3 replaceTVar vs tperm (ty dec)
          mkNewDec (dname, vs, tperm) = dec {fid=dname, ty=replaceList vs tperm}
          


auxFunc instances [] = []
auxFunc instances ((c,v):tcs) = sequence (binds :  auxFunc instances tcs) 
    where validTypes = filter ((==c) . fst) instances
          binds = map ((,) v . ConT . snd) validTypes          
            

-- | Convert type constraint into (Class, TVar)
-- NOTE: Only one type var supported
typeToTuple :: Type -> Maybe (Name, Name)
typeToTuple (AppT (ConT c) (VarT v)) = Just (c,v)
typeToTuple _ = Nothing


replaceTVar :: Name -> Type -> Type -> Type
replaceTVar v t (AppT l r) = AppT (replaceTVar v t l) (replaceTVar v t r)
replaceTVar v t (VarT n) | v == n    = t
                         | otherwise = VarT n
replaceTVar _ _ t = t


-------------------------------------------------
--                   Tests                     --
-------------------------------------------------
testParser :: String -> Name -> [Name] -> IO ()
testParser mod tname types  = do 
    (decs, failed) <- getModuleExports mod
    putStrLn "--------------- RAW ------------------"
    mapM_ print decs 
    putStrLn "----------- INSTANTIATED  ------------"
    let decs' = concatMap (instantiate types) decs 
    mapM_ print decs'
    putStrLn "------------- ACTIONS ----------------"
    let actions = filter (\d -> resultType tname d && decidableArgs d) decs' 
    mapM_ print actions
    putStrLn "------------ CONSTRAINS --------------"
    mapM_ print $ getConstraints decs'
    putStrLn "-------------- FAILED ----------------"
    mapM_ putStrLn failed




--testDevAdt :: DeriveConfig -> IO ()
--testDevAdt conf = do
--    (decs, failed) <- getModuleExports (mname conf) 
--    let tname = mkName ts
--        Right rt = parseType ts
--        rtdecs = filter (resultType rt) decs
--        rtdecs' = filter decidableArgs rtdecs
--    adt <- runQ $ devADT (monad conf) (tname conf) rtdecs'
--    print $ ppr adt
--
--
--testDevRun :: Bool -> String -> String -> IO ()
--testDevRun isMonad mod ts = do
--    (decs, failed) <- getModuleExports mod
--    let tname = mkName ts
--        Right rt = parseType ts
--        rtdecs = filter (resultType rt) decs
--        rtdecs' = filter decidableArgs rtdecs
--    run <- runQ $ devRun isMonad tname rtdecs'
--    print $ ppr run
