module Args where

import System.Console.ArgParser

data MainArgs = MainArgs String String String String Int Int String
                                    deriving(Show)

parser :: ParserSpec MainArgs
parser = MainArgs
    `parsedBy` reqPos            "type"        `Descr` "File Type to generate (e.g. Bmp, Ogg, Gif, ...)"
    `andBy`    reqPos            "name"        `Descr` "Output filename"
    `andBy`    reqPos            "command"     `Descr` "Full command line to execute"
    `andBy`    optFlag "fuzz"    "action"      `Descr` "Action to execute (zzuf | check | gen | exec)"
    `andBy`    optFlag 100000000 "tries"       `Descr` "Number of attempts"
    `andBy`    optFlag 20        "size"        `Descr` "Maximum structural size of generated values"
    `andBy`    optFlag "outdir"  "outdir"      `Descr` "Directory to dump crashes"


cli :: IO (CmdLnInterface MainArgs)
cli =
    (`setAppDescr` "An experimental grammar fuzzer in Haskell using QuickCheck.")
    <$> (`setAppEpilog` "More info: QuickFuzz.org")
    <$> mkApp parser

findFileType (MainArgs t _ _ _ _ _ _) = t
