module Args where

import System.Console.ArgParser

data MainArgs = MainArgs {findFileType :: String, findFileName ::  String, findCmds :: String, 
    findAct :: String, findNumTries:: Int, findSize :: Int, findOutDir :: String}
                                    deriving(Show)

parser :: ParserSpec MainArgs
parser = MainArgs
    `parsedBy` reqPos            "type"        `Descr` "File Type to generate (e.g. Bmp, Ogg, Gif, ...)"
    `andBy`    reqPos            "name"        `Descr` "Output filename"
    `andBy`    reqPos            "command"     `Descr` "Full command line to execute"
    `andBy`    optFlag "zzuf"    "action"      `Descr` "Action to execute (zzuf | check | gen | exec)"
    `andBy`    optFlag 100000000 "tries"       `Descr` "Number of attempts"
    `andBy`    optFlag 20        "size"        `Descr` "Maximum structural size of generated values"
    `andBy`    optFlag "outdir"  "outdir"      `Descr` "Directory to dump crashes"


cli :: IO (CmdLnInterface MainArgs)
cli =
    (`setAppDescr` "An experimental grammar fuzzer in Haskell using QuickCheck.")
    <$> (`setAppEpilog` "More info: QuickFuzz.org")
    <$> mkApp parser


splitCmd :: MainArgs -> (String, String, String)
splitCmd (MainArgs _ _ c _ _ _ _) = ret False c
    where   ret _ [] = ([],[],[])
            ret b (x:xs)    | x == '@' = if b then ([],[],xs) else ret True xs
                            | b = let (ls,rs,rss) = ret b xs in (ls, x:rs,rss)
                            | otherwise = let (ls,rs,rrs) = ret b xs in (x:ls,rs,rrs)

formatArgs :: MainArgs -> MainArgs
formatArgs args = let (hd, _, tl)  = splitCmd args in
    args {findCmds = hd ++ (findFileName args) ++ tl}
