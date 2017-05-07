{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module DeriveDispatcher (devDispatcher) where

import Language.Haskell.TH

import Args
import Formats

funName = mkName . ("dispatch"++) . nameBase

devDispatcher :: Name -> Name -> DecsQ
devDispatcher action runner = pure <$> funD (funName action) funClause
    where funClause = [clause [varP (mkName "cmd")] funBody []]
          funBody = normalB $ caseE ((varE (mkName "format")) 
                                    `appE` (varE (mkName "cmd"))) funCases
          funCases = fmtsCases ++ [unsupCase]
          unsupCase = match (varP (mkName "fmt")) 
                            (normalB ((varE (mkName "unsupported")) 
                                    `appE` (varE (mkName "fmt")))) [] 
          fmtsCases = map fmtCase formats
          fmtCase (fmt, info) = match (litP (stringL fmt)) 
                                      (normalB ((varE runner) 
                                        `appE` (varE (mkName "cmd")) 
                                        `appE` (varE info))) []

