{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.QuickFuzz.Derive.Actions where

import Debug.Trace

import Data.List.Split
import Data.List
import Data.Maybe
import Data.Tree

import Control.Monad
import Control.Applicative
import Control.Exception
import Test.QuickCheck hiding (shrinkList)

import GHC.Generics

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Language.Haskell.GhcMod
import Language.Haskell.GhcMod.Browse

import Language.Haskell.Meta.Parse

import Megadeth.Prim

-------------------------------------------------
--             Module crawler                  --
-------------------------------------------------

data Declaration = D 
    { fid   :: Name     -- Unique identifier used inside the actions adt
    , fname :: Name     -- Name of the function to be called by perform 
    , oper  :: Bool     -- The function is an infix operator? 
    , utv   :: [Name]   -- Universally quantified type vars
    , ctv   :: [Type]   -- Constrained type vars 
    , ty    :: [Type]   -- Type signature of each function parameter 
    }

instance Show Declaration where
    show d = show (fid d, fname d, oper d, utv d, ctv d, ty d)


opts = BrowseOpts 
        { optBrowseOperators = True
        , optBrowseDetailed  = True
        , optBrowseQualified = True 
       -- , optBrowseParents = False
        }



getModuleExports :: String -> IO ([Declaration], [String])
getModuleExports mod = do
    (res, log) <- runGhcModT defaultOptions $ browse opts mod
    case res of
        Left err  -> error $ "error browsing module: " ++ mod
        Right str -> return (ok, failed)
            where (ok, failed, _) = foldr parse ([],[],0) $ lines str


parse :: String -> ([Declaration],[String],Int) -> ([Declaration],[String],Int)
parse str (ok, failed, n) = 
    let parsedDecl = do
            (name, sig) <- splitDec str
            (full, isOp) <- parseName name
            ty <- parseSignature sig
            return (full, isOp, ty)
    in case parsedDecl of
        Nothing -> (ok, str:failed, n)
        Just (full, isOp, ty) -> (dec:ok, failed, n')
          where dec = D { fid=dfid, fname=full, oper=isOp
                        , utv=dutv, ctv=dctv, ty=dty }
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
    Right (UInfixE (ConE m) _ (VarE op)) -> 
        Just (mkName $ show m ++ "." ++ show op, True)
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


-------------------------------------------------
--  actions tree and perform code generation   --
-------------------------------------------------
devActions :: [String]  -- Modules to crawl 
           -> Name      -- Target type
           -> Bool      -- Monadic type?
           -> [Name]    -- Type names used for type vars instatiations
           -> [Name]  -- Ignore this declarations
           -> DecsQ 
devActions mods tname monad tinst ignored = do
    modexps <- runIO $ mapM getModuleExports mods       
    let indexed = zip (map fst modexps) [1..]          
        addModIndex (mod,n) = map (\d -> d {fid = mkSubName (fid d) n}) mod
        decs = concatMap addModIndex indexed       
        cons = getConstraints decs
    instances <- getInstances cons tinst
    mbsyn <- isSynFor tname
    let decs' = concatMap (instantiateConstraints instances) decs
        decs'' = concatMap (instantiateTVars tinst) decs'
        conditions d = and $ map ($ d) 
            [ resultType (tname, mbsyn)
            , decidableArgs
            , not . isTypeParam (tname, mbsyn)
            , not . hasFuncParams
            , not . (`elem` (map show ignored)) . show . fname]
        actions = filter conditions decs''
    adt <- devADT monad (tname,mbsyn) actions
    runner <- devPerformer monad (tname, mbsyn) actions
    shrinker <- devShrinker monad (tname, mbsyn) actions
    return [adt, runner, shrinker]




-- | Derive Arbitrary instance for the original type
-- trivially using its derived actions type
devArbitraryWithActions :: Bool   -- Monadic type?
                        -> Name   -- Target type
                        -> DecsQ
devArbitraryWithActions monadic tname =
    let sig | monadic   = appT listT (conT $ mkTypeName tname)
            | otherwise = conT $ mkTypeName tname
        perform = varE $ mkPerformerName tname
    in [d| instance Arbitrary $(conT tname) where
                arbitrary = (arbitrary :: Gen $sig) >>= return . $perform 
       |]


-- | Derive actions adt for a given type from a list of functions declarations
devADT :: Bool -> (Name, Maybe Type) -> [Declaration] -> DecQ
devADT monadic (tname, mbsyn) funcs = 
    dataD (cxt []) (mkTypeName tname) [] 
            (map (devCon monadic (tname, mbsyn)) funcs) [''Generic] 
  
devCon :: Bool -> (Name, Maybe Type) -> Declaration -> ConQ
devCon monadic (tname, mbsyn) dec = 
    normalC (mkConName tname (fid dec)) 
            (map (devBangType monadic (tname, mbsyn)) (init (ty dec)))

devBangType :: Bool -> (Name, Maybe Type) -> Type -> StrictTypeQ
devBangType monadic (tname, mbsyn) t = strictType notStrict replaceArgTy 
    where replaceArgTy | (tname, mbsyn) `compat` t  = thisType 
                       | otherwise         = return t
          thisType | monadic    = appT listT (conT $ mkTypeName tname)
                   | otherwise  = conT $ mkTypeName tname
                                      

-- | Derive actions performer for a previosly derived actions adt
devPerformer :: Bool -> (Name, Maybe Type) -> [Declaration] -> DecQ
devPerformer monadic (tname, mbsyn) actions = funD (mkPerformerName tname) clauses
    where clauses | monadic   = clausesM
                  | otherwise = clausesP
          clausesM = emptyClause : (map (devClauseM (tname,mbsyn)) actions)
          clausesP = map (devClauseP (tname, mbsyn)) actions
          emptyClause = clause [listP []] (normalB [| return () |]) []

devClauseM :: (Name, Maybe Type) -> Declaration -> ClauseQ
devClauseM (tname, mbsyn) dec = clause [consMatch] consBody []
    where cname = mkConName tname (fid dec)
          nargs = length (ty dec) - 1
          varsP = map (varP . mkVarName tname) [1..nargs]  
          varsE = map (varE . mkVarName tname) [1..nargs]
          actionP = conP cname varsP
          consMatch = infixP actionP (mkName ":") (varP $ mkVarMore tname)      
          consBody = 
            normalB (uInfixE (apply (tname, mbsyn) (dec {ty = init (ty dec)}) varsE) 
                             (varE $ mkName ">>") 
                             (appE (varE $ mkPerformerName tname) 
                                   (varE $ mkVarMore tname)))

devClauseP :: (Name, Maybe Type) -> Declaration -> ClauseQ
devClauseP (tname, mbsyn) dec = 
    clause [conP cname varsP] 
           (normalB (apply (tname, mbsyn) (dec {ty = init (ty dec)}) varsE)) []
    where cname = mkConName tname (fid dec)
          nargs = length (ty dec) - 1
          varsP = map (varP . mkVarName tname) [1..nargs]  
          varsE = map (varE . mkVarName tname) [1..nargs]

apply :: (Name, Maybe Type) -> Declaration -> [ExpQ] -> ExpQ
apply (tname, mbsyn) dec [] = funE (fname dec) 
apply (tname, mbsyn) dec vs = 
    appE (apply (tname, mbsyn) (dec {ty = init (ty dec)}) (init vs)) replaceAction 
    where replaceAction 
            | (tname, mbsyn) `compat` last (ty dec) = appE (varE (mkPerformerName tname)) (last vs)
            | otherwise                             = last vs

funE n = case parseExp $ nameBase n of
    Right (ConE _) -> conE n 
    _ -> varE n



-- | Derive actions shrinker for a previosly derived actions adt
devShrinker :: Bool -> (Name, Maybe Type) -> [Declaration] -> DecQ
devShrinker monadic (tname, mbsyn) actions = funD (mkShrinkerName tname) clauses
    where clauses = map (devShrinkClause monadic (tname, mbsyn)) actions


devShrinkClause :: Bool -> (Name, Maybe Type) -> Declaration -> ClauseQ
devShrinkClause monadic (tname, mbsyn) dec = 
    clause [conP cname varsP] (normalB subterms) []
    where cname = mkConName tname (fid dec)
          nargs = length (ty dec) - 1
          varsP = map (varP . mkVarName tname) [1..nargs]
          varsE = map (varE . mkVarName tname) [1..nargs]
          subterms = concatSubTerms cname monadic (tname, mbsyn) (ty dec) varsE


concatSubTerms :: Name -> Bool -> (Name, Maybe Type) -> [Type] -> [ExpQ] -> ExpQ
concatSubTerms cname monadic (tname, mbsyn) ts vs = [| concat $(listE subterms) |] 
    where subterms | monadic = immSubTerms ++ innerShrinks 
                   | otherwise = immSubTerms
          
          isCompat = compat (tname, mbsyn)
          
          immSubTerms = map (toList . snd) $ filter (isCompat . fst) $ zip ts vs
          
          toList v | monadic   =      v 
                   | otherwise = [| [$v] |]
          
          innerShrinks | any isCompat (init ts) = [ compE (binds ++ [result]) ]
                       | otherwise              = []
          
          binds = map (makeBind . snd) $ filter (isCompat . fst) $ zip ts (zip vs [1..])

          makeBind (v, n) = bindS (varP (mkVarName' tname n)) [| shrinkList $v |] 

          result = noBindS (apply cname (zip ts (zip vs [1..])))
          
          apply cname [] = conE cname
          apply cname xs = appE (apply cname (init xs)) (toVarE (last xs))
          toVarE (t, (v, n)) | isCompat t = varE (mkVarName' tname n) 
                             | otherwise  = v 


shrinkList :: [a] -> [[a]]
shrinkList [] = []
shrinkList xs = init (inits xs) ++ tail (tails xs)

-- List shrinker used at the FormatInfo field whe datatype is monadic
shrinkActionList :: (a -> [a]) -> [a] -> [[a]]
shrinkActionList shrinker [] = [] 
shrinkActionList shrinker xs = concat $ map replaceSubAction $ zip xs [0..]
    where replaceSubAction (act, index) = [ replaceAt index subact xs | subact <- shrinker act ]
          replaceAt index x xs = take index xs ++ [x] ++ drop (index+1) xs


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
          mkNewDec (dname, tperm) = 
            dec {fid=dname, utv=[], ty=replaceTVarList (utv dec) tperm (ty dec)}


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
          mkNewDec (dname, vs, tperm) = 
                dec {fid=dname, ty=replaceTVarList vs tperm (ty dec)}


-- | Bind a list of instances with a list of tuples (classname,varname)
-- returning a list of all permutations which satisfies class constraints
bindInstances :: [(Name,Name)] -> [(Name,Name)] -> [[(Name,Type)]]
bindInstances instances [] = []
bindInstances instances ((c,v):tcs) = sequence (binds : bindInstances instances tcs) 
    where validTypes = filter ((==c) . fst) instances
          binds = map ((,) v . ConT . snd) validTypes          


-- | Replace a list of type var names with a list of types in a declaration types list
replaceTVarList :: [Name] -> [Type] -> [Type] -> [Type]
replaceTVarList [] [] ty = ty
replaceTVarList (v:vs) (tp:tps) ty = replaceTVarList vs tps (map (replaceTVar v tp) ty)
replaceTVarList v tp _ = error $ "replaceTVar: unexpected input: " 
                               ++ show v ++ ", " ++ show tp

-- | Replace var name with type in a given type 
replaceTVar :: Name -> Type -> Type -> Type
replaceTVar v t (AppT l r) = AppT (replaceTVar v t l) (replaceTVar v t r)
replaceTVar v t (VarT n) | v == n    = t
                         | otherwise = VarT n
replaceTVar _ _ t = t



-- | Check if a type name can be applied to a class name
-- e.g.:  Monad :: (* -> *) -> *
--        Maybe :: * -> * 
--        Int   :: *
--        compatKinds ''Monad ''Maybe = True
--        compatKinds ''Monad ''Int   = False
compatKinds :: Name -> Name -> Q Bool
compatKinds cname tname = do
    ckind <- getKind cname
    tkind <- getKind tname
    case ckind of
        AppT (AppT ArrowT t) StarT -> return $ t == tkind
        _ -> return False

-- | Extract the kind asociated to a type name
getKind :: Name -> TypeQ
getKind n = do
    info <- reify n
    case info of
        ClassI (ClassD _ _ tvb _ _) _ -> makeKind tvb 
        TyConI (DataD _ _ tvb _ _) -> makeKind tvb
        TyConI (NewtypeD _ _ tvb _ _) -> makeKind tvb
        TyConI (TySynD _ tvb _) -> makeKind tvb  -- Doesn't work for every case
        t -> error $ "getKind: unexpected reify info: " ++ show t 

makeKind :: [TyVarBndr] -> TypeQ
makeKind tvb = return $ foldr (\l r -> AppT (AppT ArrowT l) r) StarT 
                      $ map toKind tvb

toKind :: TyVarBndr -> Type
toKind (PlainTV _) = StarT
toKind (KindedTV _ k) = k


-- | Convert type constraint into (Class, TVar)
-- NOTE: Only one type var supported
splitConstraint :: Type -> (Name, Name)
splitConstraint (AppT (ConT c) (VarT v)) = (c,v)
splitConstraint t = error $ "splitConstraint: unexpected input: " ++ show t

-- | Extract all type constraint names from a declaration batch
getConstraints :: [Declaration] -> [Name]
getConstraints = rmdups . map (fst . splitConstraint) . concatMap ctv

-- | The given declaration contains type vars?
decidableArgs :: Declaration -> Bool
decidableArgs dec = all (not . hasVarT) (ty dec)

hasVarT :: Type -> Bool
hasVarT (VarT _) = True
hasVarT (AppT l r) = hasVarT l || hasVarT r
hasVarT (ConT _) = False
hasVarT _ = False


-- | Compare type name and the result type of a declaration
resultType :: (Name, Maybe Type) -> Declaration -> Bool
resultType (tname, mbsyn) dec = (tname, mbsyn) `compat` last (ty dec)


compat :: (Name, Maybe Type) -> Type -> Bool
compat (tname, Just syn) t | syn == t  =  True
compat (tname, mbsyn) (AppT l r) = compat (tname, mbsyn) l
compat (tname, mbsyn) (ConT c)   = unqualify tname == unqualify c
compat _ _ = False


-- Check if a name is a type synonym of another
isSynFor :: Name -> Q (Maybe Type)
isSynFor tname = do
    tinfo <- reify tname
    case tinfo of
        TyConI (TySynD syn _  (AppT (ConT n1) (ConT n2))) -> 
            return (Just (ConT (unqualify n1) `AppT` ConT (unqualify n2)))
        _ -> return Nothing



-- | Check if a type name is a type parameter of another
-- e.g.: isTypeParam ''Int (AppT (ConT ''Maybe) (ConT ''Int)) = True
--       isTypeParam ''Int (ConT ''Int) = False
isTypeParam :: (Name, Maybe Type) -> Declaration -> Bool
isTypeParam (tname, mbsyn) dec = 
    any (\t -> not ((tname, mbsyn) `compat` t) && isContained (tname, mbsyn) t) (ty dec)


isContained (tname, Just syn) t | syn == t  =  True
isContained (tname, mbsyn) (AppT l r) = isContained (tname, mbsyn) l || isContained (tname, mbsyn) r
isContained (tname, mbsyn) (ConT t) = unqualify tname == unqualify t
isContained  _ _ = False
            


-- | Check if a declaration has functions as a parameter
hasFuncParams :: Declaration -> Bool
hasFuncParams dec = any hasArrows (ty dec)

hasArrows (AppT l r) = hasArrows l || hasArrows r
hasArrows ArrowT = True
hasArrows _ = False


-------------------------------------------------
--                  Helpers                    --
-------------------------------------------------

-- | Name manipulations
mkTypeName tname = mkName $ nameBase tname ++ "Action"
mkPerformerName tname = mkName $ "perform" ++ nameBase tname ++ "Action"
mkShrinkerName tname = mkName $ "shrink" ++ nameBase tname ++ "Action"

mkSubName f n = mkName $ nameBase f ++ "_" ++ show n
mkOpName n = mkName $ "infix" ++ show n
mkConName tname f = mkName $ "Act_" ++ nameBase tname ++ "_" ++ nameBase f
mkVarName tname v = mkName $ "v" ++ show v ++ "_" ++ nameBase tname
mkVarName' tname v = mkName $ "v" ++ show v ++ "'_" ++ nameBase tname
mkVarMore tname = mkName $ "more" ++ nameBase tname ++ "Actions"

unqualify :: Name -> Name
unqualify = mkName . nameBase


-- | Remove duplicates
rmdups :: (Ord a, Eq a) => [a] -> [a]
rmdups = map head . group . sort
