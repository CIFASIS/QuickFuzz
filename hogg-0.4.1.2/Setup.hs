#!/usr/bin/env runhaskell

import Distribution.Simple (defaultMainWithHooks, simpleUserHooks,
                             UserHooks(..), Args)

{-
-- The test-related options have been disabled due to incompitibilities
-- in various Cabal release versions. In particular, the type expected for
-- runTests has changed, and joinPaths was not exported in Cabal 1.1.6
import           Distribution.PackageDescription (PackageDescription)
import           Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import qualified Distribution.Simple.LocalBuildInfo as LBI
import           Distribution.Simple.Utils (rawSystemVerbose)
import           Distribution.Compat.FilePath (joinPaths)
import System.Exit(ExitCode(..))
-}

main :: IO ()
main = defaultMainWithHooks (simpleUserHooks)
-- main = defaultMainWithHooks (defaultUserHooks{runTests = tests})

{-
-- Definition of tests for Cabal 1.1.3
tests :: Args -> Bool -> LocalBuildInfo -> IO ExitCode
tests args _ lbi =
    let testCmd = foldl1 joinPaths [LBI.buildDir lbi, "ListMergeTest", "ListMergeTest"]
    in  rawSystemVerbose 1 testCmd
        ("+RTS" : "-M32m" : "-c30" : "-RTS" : args)
-}

{-
-- Definition of tests for Cabal 1.1.7
tests :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ExitCode
tests args _ _ lbi =
    let testCmd = foldl1 joinPaths [LBI.buildDir lbi, "ListMergeTest", "ListMergeTest"]
    in  rawSystemVerbose 1 testCmd
        ("+RTS" : "-M32m" : "-c30" : "-RTS" : args)
-}
