{-# LANGUAGE RankNTypes, ScopedTypeVariables, RecordWildCards, NamedFieldPuns, ConstraintKinds,  FlexibleContexts, DataKinds, KindSignatures, TypeOperators, ViewPatterns #-} 


module ModuleFinder where

--import Safe
import Control.Applicative
import Control.Exception (SomeException(..))
import Data.Char
import Data.List
import Data.Maybe
import FastString
import GHC
import qualified GHC as G
--import Language.Haskell.GhcMod.Convert
--import Language.Haskell.GhcMod.Gap as Gap
--import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Monad
--import Language.Haskell.GhcMod.Logging
import Name (getOccString)
--import Outputable
--import TyCon (isAlgTyCon)
--import Type (dropForAlls, splitFunTy_maybe, mkFunTy, isPredTy)
import Exception (ExceptionMonad)
--import Prelude
mport Language.Haskell.GhcMod hiding (test)
import Language.Haskell.GhcMod.Exports 
import Outputable
import OccName
import Name

--finder mod = do (mres, log) <- runGhcModT defaultOptions $ test mod
--                case mres of
--                                  Left err -> putStrLn "Error"
--                                                    Right xs -> print (map printTuple xs)
--                                                               
--                                                               printTuple (name, _ ,_) = show name
--                                                               
--                                                               instance Show Name where
--                                                                   show = getOccString 
--                                                                   
--                                                                   
--                                                                   --main = do (Right res,_ ) <- runGhcModT defaultOptions $ test "Data.Maybe"
--                                                                   ----          print "ok"
--                                                                   --"""")))""))
------------------------------------------------------------------

test :: forall m. IOish m => String -> GhcModT m [(Name, Module, Maybe TyThing)] 
test pkgmdl = goPkgModule `G.gcatch` (\(SomeException _) -> goHomeModule)
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
                    
