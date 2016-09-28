module ModuleParser where

import Language.Haskell.Exts

import Language.Haskell.GhcMod
import Language.Haskell.GhcMod.Browse
import Language.Haskell.GhcMod.Monad


opts = BrowseOpts { optBrowseOperators = True
                  , optBrowseDetailed  = True
                  , optBrowseQualified = False
                  }


type ExpFunc = ([Name], Type)

getModuleExports :: String -> IO ([ExpFunc], [String])
getModuleExports mod = do
    (res, log) <- runGhcModT defaultOptions $ browse opts mod
    case res of
        Left err  -> return ([], [])
        Right str -> return $ foldr parse ([],[]) $ lines str
                        where parse str (ok, failed) = 
                                 case parseDecl str of
                                    ParseOk (TypeSig l n t) -> ((n,t):ok, failed)
                                    _ -> (ok, str:failed)

testParser :: String -> IO ()
testParser mod = do 
    res <- getModuleExports mod
    case res of
        ([],[]) -> error "error retrieving module info"
        (decs, failed) -> do putStrLn "----------------------------"
                             mapM_ (putStrLn . show) decs 
                             putStrLn "----------------------------"
                             mapM_ putStrLn failed
