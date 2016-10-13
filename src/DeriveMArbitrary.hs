{-# LANGUAGE TemplateHaskell #-}

module DeriveMArbitrary where

import Data.List.Split
import Data.List
import Data.Maybe

import Control.Monad
import Control.Exception

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Language.Haskell.GhcMod
import Language.Haskell.GhcMod.Browse

import Language.Haskell.Meta.Parse

-------------------------------------------------
--             Module crawler                  --
-------------------------------------------------
type Declaration = (Name, [Name], Cxt, [Type])

opts = BrowseOpts { optBrowseOperators = True
                  , optBrowseDetailed  = True
                  , optBrowseQualified = True }

getModuleExports :: String -> IO ([Declaration], [String])
getModuleExports mod = do
    (res, log) <- handle (\e -> error (show (e :: SomeException))) $ runGhcModT defaultOptions $ browse opts mod
    case res of
        Left err  -> error $ "error browsing module: " ++ mod
        Right str -> return $ foldr parse ([],[]) $ lines str


parse str (ok, failed) = 
    let [fname, sig] = splitOn " :: " str
        (qual, name) = case splitOn "(" fname of
                            [name] -> let qname = splitOn "." name
                                      in (intercalate "." $ init qname, last qname)
                            [q,op] -> (q, "("++op)
                            _ -> error "unexpected string"
    in
        case parseDecs (name ++ "::" ++ sig) of
            Right [SigD _ ty] -> ((flattenDec (mkName $ qual ++ "." ++ name) ty):ok, failed)
            _ -> (ok, str:failed)


flattenDec :: Name -> Type -> Declaration
flattenDec name (ForallT tvb cxt ty) = 
    let (_, tvb', cxt', ty') = flattenDec name ty
    in (name, map fromTyVarBndr tvb ++ tvb', cxt ++ cxt', ty')
flattenDec name (AppT (AppT ArrowT l) r) = 
    let (_, tvb', cxt', r') = flattenDec name r
    in (name, tvb', cxt', [l] ++ r')
flattenDec name t = (name, [], [], [t])
    
fromTyVarBndr :: TyVarBndr -> Name
fromTyVarBndr (PlainTV n) = n
fromTyVarBndr (KindedTV n _) = n


--tryFixParse :: String -> Maybe Declaration
--tryFixParse str = case splitOn " :: " str of
--    [qname, sig] -> case splitOn "(" qname of
--        [qual, op] -> case parseExp ("("++op++" :: "++sig) of
--                        Right (SigE (VarE name) ty) -> 
--                                let newName = mkName $ qual ++ "(" ++ nameBase name ++ ")" 
--                                in Just (flattenDec newName ty)
--                        _ -> Nothing
--        _ -> Nothing
--    _ -> Nothing

-------------------------------------------------
--  actions tree and perform code generation   --
-------------------------------------------------
data DeriveConfig = DeriveConfig
                    { mod      :: String
                    , tname    :: Name
                    , isMonad  :: Bool
                    , auxTypes :: [Name]
                    } deriving Show  



devMArbitrary :: Bool -> [Name] -> String -> Name -> Q [Dec]
devMArbitrary isMonad aux mod tname = do
    (decs, failed) <- runIO $ getModuleExports mod
    let decs' = concat $ map (instantiate aux) decs 
        actions = filter (\d -> resultType (toType tname) d && decidableArgs d) decs' 
--    isMonad <- isInstance ''Monad [toType tname]
    adt <- devADT isMonad tname actions
    run <- devRun isMonad tname actions
    return [adt, run]


devADT :: Bool -> Name -> [Declaration] -> DecQ
devADT isMonad tname funcs = 
    dataD (cxt []) (mkTypeName tname) [] (map (devCon isMonad tname) funcs) [] 

devCon :: Bool -> Name -> Declaration -> ConQ
devCon isMonad tname (n, tv, cxt, ty) = 
    normalC (mkConName tname n) (map (devBangType isMonad tname) (init ty))

devBangType :: Bool -> Name -> Type -> StrictTypeQ
devBangType isMonad tname t = strictType notStrict replaceArgTy 
    where replaceArgTy | toType tname `compat` t  = thisType 
                       | otherwise                = return t
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
devClauseM tname (n,tvb,cxt,ts) = clause [consMatch] consBody []
    where cname = mkConName tname  n
          varsP = map (varP . mkVarName tname) [1..(length ts - 1)]  
          varsE = map (varE . mkVarName tname) [1..(length ts - 1)]
          actionP = conP cname varsP
          consMatch = infixP actionP (mkName ":") (varP $ mkVarMore tname)      
          consBody = (normalB (uInfixE (apply tname (n,tvb,cxt,init ts) varsE) 
                              (varE $ mkName ">>") 
                              (appE (varE $ mkPerformName tname) (varE $ mkVarMore tname))))

devClauseP :: Name -> Declaration -> ClauseQ
devClauseP tname (n,tvb,cxt,ts) = clause [conP cname varsP] (normalB (apply tname (n,tvb,cxt, init ts) varsE)) []
    where cname = mkConName tname  n
          varsP = map (varP . mkVarName tname) [1..(length ts - 1)]  
          varsE = map (varE . mkVarName tname) [1..(length ts - 1)]

    
apply :: Name -> Declaration -> [ExpQ] -> ExpQ
apply tname (n,_,_,_) [] = varE n 
apply tname (n,tvb,cxt,ts) vs = appE (apply tname (n,tvb,cxt,init ts) (init vs)) replaceAction 
    where replaceAction | toType tname `compat` last ts  = appE (varE (mkPerformName tname)) (last vs)
                        | otherwise                      = last vs
          


toType = ConT . mkName . nameBase 

mkTypeName tname = mkName $ nameBase tname ++ "Action"
mkConName tname f = mkName $ "Act_" ++ nameBase tname ++ "_" ++ nameBase f
mkVarName tname v = mkName $ "v" ++ show v ++ "_" ++ nameBase tname
mkPerformName tname = mkName $ "perform" ++ nameBase tname
mkVarMore tname = mkName $ "more" ++ nameBase tname ++ "Actions"
mkFunSubName f n = let Just mod = nameModule f
                   in mkName $ mod ++ "." ++ nameBase f ++ "_" ++ show n

resultType :: Type -> Declaration -> Bool
resultType rt (_,_,_,ty) = rt `compat` last ty

compat :: Type -> Type -> Bool
compat t (AppT l r)  = compat t l
compat (ConT c) (ConT c') =  c == c'  
compat _ _ = False

decidableArgs :: Declaration -> Bool
decidableArgs (_,_,_,ty) = all (not . hasVarT) ty

hasVarT :: Type -> Bool
hasVarT (VarT _) = True
hasVarT (AppT l r) = hasVarT l || hasVarT r
hasVarT (ConT _) = False
hasVarT _ = False

replaceTVar :: Name -> Type -> Type -> Type
replaceTVar v t (AppT l r) = AppT (replaceTVar v t l) (replaceTVar v t r)
replaceTVar v t (VarT n) | v == n    = t
                         | otherwise = VarT n
replaceTVar _ _ t = t

instantiate :: [Name] -> Declaration -> [Declaration]
instantiate tnames dec@(_,[],_,_) = [dec]
instantiate tnames (n, tvb, cxt, ty) = map (\(n', perm) -> (n', [], cxt, instantiateList tvb perm ty)) tuples
    where tperms = replicateM (length tvb) (map ConT tnames)
          names = zipWith mkFunSubName (replicate (length tperms) n) [1..]
          tuples = zip names tperms
        
instantiateList :: [Name] -> [Type] -> [Type] -> [Type]
instantiateList [] [] types = types
instantiateList (v:vs) (t:ts) types = instantiateList vs ts $ map (replaceTVar v t) types


-------------------------------------------------
--                   Tests                     --
-------------------------------------------------
testParser :: [Name] -> String -> Name -> IO ()
testParser aux mod tname  = do 
    (decs, failed) <- getModuleExports mod
    putStrLn "------ RAW ------------------"
    mapM_ print decs 
    let decs' = concat $ map (instantiate aux) decs 
        actions = filter (\d -> resultType (toType tname) d && decidableArgs d) decs' 
    putStrLn "------- DECS ---------------------"
    mapM_ print decs' 
    putStrLn "-------- ACTIONS --------------------"
    mapM_ print actions 
    putStrLn "-------- FAILED --------------------"
    mapM_ putStrLn failed

testDevAdt :: Bool -> String -> String -> IO ()
testDevAdt isMonad mod ts = do
    (decs, failed) <- getModuleExports mod
    let tname = mkName ts
        Right rt = parseType ts
        rtdecs = filter (resultType rt) decs
        rtdecs' = filter decidableArgs rtdecs
    adt <- runQ $ devADT isMonad tname rtdecs'
    print $ ppr adt


testDevRun :: Bool -> String -> String -> IO ()
testDevRun isMonad mod ts = do
    (decs, failed) <- getModuleExports mod
    let tname = mkName ts
        Right rt = parseType ts
        rtdecs = filter (resultType rt) decs
        rtdecs' = filter decidableArgs rtdecs
    run <- runQ $ devRun isMonad tname rtdecs'
    print $ ppr run
