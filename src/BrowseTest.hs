--import Control.Applicative ((<$>))
--import Control.Exception (SomeException(..))
--import Data.Char (isAlpha)
--import Data.List (sort)
--import Data.Maybe (catMaybes)
----import Exception (ghandle)
----import FastString (mkFastString)
--import GHC (GhcException(CmdLineError), ModuleInfo, Name, TyThing, DynFlags, Type, TyCon)
--import qualified GHC as G
--import Outputable
--
--import Language.Haskell.GhcMod
--import Language.Haskell.GhcMod.Browse
--import Language.Haskell.GhcMod.Convert
--import Language.Haskell.GhcMod.Doc (showPage, styleUnqualified)
--import Language.Haskell.GhcMod.Gap
--import Language.Haskell.GhcMod.Monad (GhcModT, options)
----import Language.Haskell.GhcMod.Target (setTargetFiles)
--import Language.Haskell.GhcMod.Types
--
--opts = BrowseOpts { optBrowseOperators = False
--                  , optBrowseDetailed = True
--                  , optBrowseQualified = False 
--                  }
--
--testBrowse mod = do 
--    (res, log) <- runGhcModT defaultOptions (browse opts mod)
--    case res of
--        Right x -> putStrLn x
--        Left err -> putStrLn $ show err 

import GHC
import Outputable
 
import GHC.Paths ( libdir )
--GHC.Paths is available via cabal install ghc-paths
 
import DynFlags
targetFile = "Pixel.hs"
 
main :: IO ()
main = do
   res <- example
   str <- runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        return $ showSDoc dflags $ ppr res
   putStrLn str
 
example = 
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      runGhc (Just libdir) $ do
        
        dflags <- getSessionDynFlags
        let dflags' = foldl xopt_set dflags
                            [Opt_Cpp, Opt_ImplicitPrelude, Opt_MagicHash]
        setSessionDynFlags dflags'
        
        target <- guessTarget targetFile Nothing
        setTargets [target]
        load LoadAllTargets
        
        modSum <- getModSummary $ mkModuleName "Pixel"
        p <- parseModule modSum
 
        return $ parsedSource p 


--test = do
--    xs <- decls
--    mapM print xs 

--test2 = do 
--    --res <- G.findModule (mkModuleName  "Data.List") Nothing
--    --sum <- G.getModSummary $ mkModuleName "Data.List"
--    --g <- G.getModuleGraph
--    --mapM G.showModule g 
--    G.runGhc Nothing $ do
--        modSum <- G.getModSummary $ G.mkModuleName "B"
--    --p <- G.parseModule modSum
--    --t <- G.typecheckModule p
--    --d <- G.desugarModule t
--    --l <- G.loadModule d
--    --n <- G.getNamesInScope
--    --c <- return $ G.coreModule d
-- 
--    --g <- G.getModuleGraph
--        mapM G.getModuleGraph modSum     
--            
    
    
    --putStrLn "YAY" 
    --putStrLn $ show (G.parsedSource d,"/n-----/n",  G.typecheckedSource d)
-- | Getting functions, classes, etc from a module.
--   If 'detailed' is 'True', their types are also obtained.
--   If 'operators' is 'True', operators are also returned.
--browse2 :: IOish m
--       => ModuleString -- ^ A module name. (e.g. \"Data.List\")
--       -> GhcModT m String
--browse2 pkgmdl = convert' . sort =<< (listExports =<< getModule)
--  where
--    (mpkg,mdl) = splitPkgMdl pkgmdl
--    mdlname = G.mkModuleName mdl
--    mpkgid = mkFastString <$> mpkg
--    listExports Nothing       = return []
--    listExports (Just mdinfo) = processExports mdinfo
--    -- findModule works only for package modules, moreover,
--    -- you cannot load a package module. On the other hand,
--    -- to browse a local module you need to load it first.
--    -- If CmdLineError is signalled, we assume the user
--    -- tried browsing a local module.
--    getModule = browsePackageModule `G.gcatch` fallback `G.gcatch` handler
--    browsePackageModule = G.findModule mdlname mpkgid >>= G.getModuleInfo
--    browseLocalModule = ghandle handler $ do
--      setTargetFiles [mdl]
--      G.findModule mdlname Nothing >>= G.getModuleInfo
--    fallback (CmdLineError _) = browseLocalModule
--    fallback _                = return Nothing
--    handler (SomeException _) = return Nothing
