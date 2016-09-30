{-# LANGUAGE RankNTypes, ScopedTypeVariables, RecordWildCards, 
    NamedFieldPuns, ConstraintKinds, FlexibleContexts, 
    DataKinds, KindSignatures, TypeOperators, ViewPatterns #-} 

module ModuleFinder where

import Control.Applicative
import Control.Exception (SomeException(..))
import Data.Char
import Data.List
import Data.Maybe
import FastString
import GHC
import qualified GHC as G
import Language.Haskell.GhcMod.Monad
import Name (getOccString)
import Exception (ExceptionMonad)
import Language.Haskell.GhcMod

type ExportedThing = (Name, Module, TyThing)

--type FunName = String
--type TypeName = String
--type ConsName = String
--
--data SimplyTyped = Fun  FunName  [TypeName] TypeName
--                 | Cons ConsName [TypeName] TypeName
--                   deriving (Show, Eq)
--
--
--transTyThing :: ExportedThing -> Maybe SimplyTyped
--transTyThing (n, m, AnId id) = do (ts, rt) <- transId id
--                                  return $ Fun (getOccString n) ts rt
--transTyThing (n, m, ATyCon t) = do (ts, rt) <- transTyCon t
--                                   return $ Cons (getOccString n) ts rt
--transTyThing (n, m, AConLike cl) = do (ts, rt) <- transConLike cl
--                                      return $ Cons (getOccString n) ts rt
--transTyThing _ = Nothing
--
---- Translate Ids
--transId id = case idType id of
--    FunTy k1 k2 -> case k2 of
--        TyConApp tc [] -> do 
--            its <- transInterm k1
--            return (its, getOccString $ getName tc)
--        _ -> Nothing
--    TyConApp tc tl -> case tl of
--        [] -> return ([], getOccString $ getName tc)
--        _  -> Nothing 
--    TyVarTy tv -> Nothing
--    ForAllTy v t -> Nothing
--    _   -> error "broken kind invariant"
--
--
--transInterm (TyConApp tc []) = Just [getOccString $ getName tc]
--transInterm (FunTy k1 (TyConApp tc [])) = do its <- transInterm k1
--                                             return $ its ++ [getOccString $ getName tc]
--transInterm _ = Nothing
--
--
--transTyConApp tc [] = Just ([], getOccString $ getName tc)
--transTyConApp _  _  = Nothing
--
--
---- Translate TyCons
--transTyCon t = case tyConKind t of
--    FunTy k1 k2 -> case k2 of
--        TyConApp tc [] -> do
--            its <- transInterm k1
--            return (its, getOccString $ getName tc)
--        _ -> Nothing
--    TyConApp tc tl -> case tl of
--        [] -> return ([], getOccString $ getName tc)
--        _  -> Nothing
--    TyVarTy tv -> Nothing 
--    ForAllTy v t -> Nothing
--    _ -> error "broken kind invariant"
--
---- Translate ConLikes
--transConLike (RealDataCon dc) = 
--    let (_,_,ts,rt) = dataConSig dc
--    in case rt of
--        TyConApp tc ts -> return (["--------"], getOccString $ getName tc)
--        _ -> Nothing
--
---- Test
--testSimplyTyped mod = do exps <- exported mod
--                         mapM_ (\exp@(n,m,tt) -> print (getOccString n, transTyThing exp)) exps
--

-------------------------------------------------------------------------------------
--testFinder mod = do mods <- exported mod
--                    print $ map printTuple mods
--
--printTuple (name, mod, tyth) = (getOccString name, printTyThing tyth)
--                                    where printTyThing :: TyThing -> String
--                                          printTyThing (AnId id)    = printId id 
--                                          printTyThing (AConLike cl) = "AConLike" 
--                                          printTyThing (ATyCon t)    = "ATyCon" 
--                                          printTyThing (ACoAxiom ca) = "ACoAxion" 
--
showOut :: Outputable a => a -> String
showOut = showSDocUnsafe . ppr
--
--idToList (FunTy k1 k2) = showOut k1 : idToList k2
--idToList (TyConApp tc ts) = [showOut tc] ++ map (concat . idToList) ts 
--idToList (TyVarTy v) = [showOut v]
--idToList (ForAllTy v ty) = [showOut v] ++ idToList ty 
--
--printId id = case  dropForAlls (idType id) of
--                FunTy k1 k2 -> "FunTy: " ++ show (idToList (FunTy k1 k2))
--                TyConApp tc ts -> "TyConApp " ++ show (idToList (TyConApp tc ts ))
--                TyVarTy v -> "TyVarTy " ++ show (idToList (TyVarTy v))
--                ForAllTy v ty -> "ForAllTy " ++  show (idToList (ForAllTy v ty)) 

-------------------------------------------------------------------------------------


testExported :: String -> IO ()
testExported mod = do exps <- exported mod
                      print $ map printTuple exps
                        where printTuple :: ExportedThing -> (String, String)
                              printTuple (n,m,tt) = (getOccString n, showSDocUnsafe $ ppr tt)

exported :: String -> IO [ExportedThing]
exported mod = do (mres, log) <- runGhcModT defaultOptions $ finder mod
                  case mres of
                    Left err -> error $ "Error calling ghc-mod: " ++ show err 
                    Right xs -> return $ foldr filterMaybe [] xs
                                    where filterMaybe (n,m,Just t) rest = (n,m,t) : rest 
                                          filterMaybe _            rest = rest

                                                                   
finder :: forall m. IOish m => String -> GhcModT m [(Name, Module, Maybe TyThing)] 
finder pkgmdl = goPkgModule `G.gcatch` (\(SomeException _) -> goHomeModule)
        where
           goPkgModule = do
             runGmPkgGhc $ do
               mod <- G.findModule mdlname mpkgid
               maymod <- G.getModuleInfo mod
               case maymod of
                    Just modinfo -> listExports modinfo
                    Nothing -> error "goPgkModule"
       
           goHomeModule = runGmlT [Right mdlname] $ do
               mod <- G.findModule mdlname Nothing
               maymod <- G.getModuleInfo mod
               case maymod of
                    Just modinfo -> listExports modinfo
                    Nothing -> error "goHomeModule"
       
           (mpkg, mdl) = splitPkgMdl pkgmdl
           mdlname = G.mkModuleName mdl
           mpkgid = mkFastString <$> mpkg

splitPkgMdl :: String -> (Maybe String,String)
splitPkgMdl pkgmdl =
  case break (==':') pkgmdl of
    (mdl, "")    -> (Nothing, mdl)
    (pkg, _:mdl) -> (Just pkg, mdl)

listExports :: (G.GhcMonad m, MonadIO m, ExceptionMonad m) => ModuleInfo -> m [(Name, Module, Maybe TyThing)] 
listExports minfo = do
            mapM (transName minfo) $ G.modInfoExports minfo
                where transName minfo name = do 
                            mtype' <- mtype name
                            return (name, G.nameModule name, mtype')
                      mtype :: GhcMonad m => Name -> m (Maybe TyThing)
                      mtype name = do
                            tyInfo <- G.modInfoLookupName minfo name
                            tyResult <- maybe (inOtherModule name) (return . Just) tyInfo
                            return tyResult
                      inOtherModule :: GhcMonad m => Name -> m (Maybe TyThing)
                      inOtherModule nm = do
                             G.getModuleInfo (G.nameModule nm) >> G.lookupGlobalName nm
                    
