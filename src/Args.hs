module Args where

import System.Console.ArgParser
import Data.List.Split
import Data.List
import Data.Char

data MainArgs = MainArgs
                        {findFileType :: String
                        ,findCmds     :: String
                        ,findFileName :: String
                        ,findAct      :: String
                        --,findCoefFile :: String
                        ,findNumTries :: Int
                        ,findSize     :: Int
                        ,findOutDir   :: String
                        ,findInDir   :: String
                       ,findPar      :: Bool}
    deriving(Show)


parser :: ParserSpec MainArgs
parser = MainArgs
    `parsedBy` reqPos            "type"        `Descr` "File Type to generate (e.g. Bmp, Ogg, Gif, ...)"
    `andBy`    reqPos            "command"     `Descr` "Full command line to execute"
    `andBy`    optFlag []        "name"        `Descr` "Output filename"
    `andBy`    optFlag "exec"    "action"      `Descr` "Action to execute (zzuf | check | gen | exec | mut | serve)"
    --`andBy`    optFlag ""        "coefs"       `Descr` "File containing probability coefficients"
    `andBy`    optFlag 100000000 "tries"       `Descr` "Number of attempts"
    `andBy`    optFlag 50        "size"        `Descr` "Maximum structural size of generated values"
    `andBy`    optFlag "outdir"  "outdir"      `Descr` "Directory to dump crashes"
    `andBy`    optFlag ""        "indir"       `Descr` "Directory to look for seeds"
    `andBy`    boolFlag          "parallel"    `Descr` "Activate parallel execution"


cli :: IO (CmdLnInterface MainArgs)
cli =
    (`setAppDescr` "An experimental grammar fuzzer in Haskell using QuickCheck.")
    <$> (`setAppEpilog` "More info: QuickFuzz.org")
    <$> mkApp parser


splitCmd :: MainArgs -> Maybe (String, String)
splitCmd args = if "@@" `isInfixOf` (findCmds args)
                    then case splitOn "@@" (findCmds args) of
                            [l,r] -> Just (l, r) 
                            _ -> error "bad command"
                    else Nothing

formatFileName :: MainArgs -> String -> MainArgs
formatFileName args filename = 
    args {findFileName = (filename ++ '.': (map toLower (findFileType args)))}

formatArgs :: MainArgs -> (String -> MainArgs)
formatArgs args = case (splitCmd args, findFileName args) of
                    (Just (l,r), name) -> \x -> args {findCmds = l ++ x ++ name ++ r}
                    _   -> \x -> args {findFileName = ""}

