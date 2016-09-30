{-# LANGUAGE TemplateHaskell #-}

module DeriveMArbitrary where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Language.Haskell.GhcMod
import Language.Haskell.GhcMod.Browse
import Language.Haskell.GhcMod.Monad

import Language.Haskell.Exts as Ext hiding (Name)


devMArbitrary :: String -> String -> Q [Dec]
devMArbitrary mod tname = do
    (res, _) <- runIO $ getModuleExports mod
    let ty = fromParseResult (parseType tname)
        actions =  filter (resultType ty . snd) res 
        
    adt <- runQ $ devADT tname actions 
    run <- devRun tname actions
    return $  [adt, run]


devRun :: String -> [(String, Ext.Type)] -> DecQ
devRun tname actions = do
    clauses <- mapM devClause actions
    return $ FunD (mkName $ "run_Action_" ++ tname) clauses

--   [FunD runHtmlAction_7 [Clause [ConP Html.Run_body [VarP inner_8]] (NormalB (AppE (VarE Text.Blaze.Html5.body) (AppE (VarE runHtmlAction_7) (VarE inner_8)))) []]]
devClause :: (String, Ext.Type) -> ClauseQ
devClause (n,t) = do
    let cname = mkConName n
        args = init $ flattenType t
        vars = map (\n -> varP (mkName $ "v" ++ show n)) [1..(length args)]
    pat <- conP cname vars 
    clause [conP cname vars] (normalB (litE (IntegerL 1))) []
    

mkTypeName n = mkName $ "Action_" ++ n
mkConName n = mkName $ "Run_" ++ n


devADT :: String -> [(String, Ext.Type)] -> DecQ
devADT tname funcs = do
    --let nm' = filter isAlphaNum nm
    --    dName = [toUpper $ head nm'] ++ tail nm' ++ "Action"
    cons <- mapM (devCon tname) funcs
    return $ DataD [] (mkTypeName tname) [] cons [] 


devCon :: String -> (String, Ext.Type) -> ConQ
devCon tname (n, t) = do
    let ty = fromParseResult (parseType tname)
        act_ty = fromParseResult (parseType $ "Action_" ++ tname)
    args <- mapM devBangType $ map (replace ty act_ty) $ init $ flattenType t 
    return $ NormalC (mkConName n) args  

devBangType :: Ext.Type -> StrictTypeQ
devBangType t = strictType notStrict (transType t) 

replace :: Ext.Type -> Ext.Type -> Ext.Type -> Ext.Type
replace ty act_ty t      | t == ty  = act_ty
                         | otherwise  = undefined

transType :: Ext.Type -> TypeQ
transType (TyCon qname) = transTyCon qname
transType _ = undefined


transTyCon :: QName -> TypeQ
transTyCon (UnQual (Ident id)) = conT (mkName id) 
transTyCon _ = undefined



-------------------------------------------------
--             Module crawler                  --
-------------------------------------------------
opts = BrowseOpts { optBrowseOperators = True
                  , optBrowseDetailed  = True
                  , optBrowseQualified = False
                  }



getModuleExports :: String -> IO ([(String, Ext.Type)], [String])
getModuleExports mod = do
    (res, log) <- runGhcModT defaultOptions $ browse opts mod
    case res of
        Left err  -> return ([], [])
        Right str -> return $ foldr parse ([],[]) $ lines str
                        where parse str (ok, failed) = 
                                 case Ext.parseDecl str of
                                    ParseOk (TypeSig l [Ident n] t) -> ((n,t):ok, failed)
                                    ParseOk (TypeSig l [Symbol n] t) -> ((n,t):ok, failed)
                                    _ -> (ok, str:failed)


resultType :: Ext.Type -> Ext.Type -> Bool
resultType rt (TyFun t1 t2) = resultType rt t2
resultType rt t = rt == t 



flattenType :: Ext.Type -> [Ext.Type]
flattenType (TyFun t1 t2) = t1 : flattenType t2
flattenType t = [t]

------------------------------------------------------------------------

--testDevADT mod ts = do
--    (res, _) <- getModuleExports mod
--    let ty = fromParseResult (parseType ts)
--        actions = filter (resultType ty . snd) res 
--    dec <- runQ $ (do tn <- newName "Action"
--                      devADT tn actions) 
--    print $ ppr dec 
