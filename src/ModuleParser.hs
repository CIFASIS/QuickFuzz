{-# LANGUAGE TemplateHaskell, GADTs #-}

module ModuleParser where

import Data.Char

import Language.Haskell.GhcMod
import Language.Haskell.GhcMod.Browse
import Language.Haskell.GhcMod.Monad

import Language.Haskell.Exts as Ext

import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax

import Text.Blaze.Html


devADT :: String -> [(String, Ext.Type)] -> DecQ
devADT nm funcs = do
    let nm' = filter isAlphaNum nm
        dName = [toUpper $ head nm'] ++ tail nm' ++ "Action"
    cons <- mapM devCon funcs
    return $ DataD [] (mkName dName) [] cons [] 


devCon :: (String, Ext.Type) -> ConQ
devCon (n, t) = do
    args <- mapM devBangType $ init $ flattenType t 
    return $ NormalC (mkName $ "Act_"++n) args  

devBangType :: Ext.Type -> StrictTypeQ
devBangType t = strictType notStrict (transType t) 


transType :: Ext.Type -> TypeQ
transType (TyCon qname) = transTyCon qname
transType _ = undefined


transTyCon :: QName -> TypeQ
transTyCon (UnQual (Ident id)) = conT (mkName id) 
transTyCon _ = undefined



-----------------------------------------------------------------------
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

testDevADT mod ts = do
    (res, _) <- getModuleExports mod
    let ty = fromParseResult (parseType ts)
        actions = filter (resultType ty . snd) res 
    dec <- runQ $ devADT ts actions 
    print $ ppr dec 

testFilterRT mod ts = do
    (res, _) <- getModuleExports mod
    let ty = fromParseResult (parseType ts)
        actions = filter (resultType ty . snd) res 
    mapM_ (putStrLn . show) actions


testParser :: String -> IO ()
testParser mod = do 
    res <- getModuleExports mod
    case res of
        ([],[]) -> error "error retrieving module info"
        (decs, failed) -> do putStrLn "----------------------------"
                             mapM_ (putStrLn . show) decs 
                             putStrLn "----------------------------"
                             mapM_ putStrLn failed
