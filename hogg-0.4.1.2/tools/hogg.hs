{-# LANGUAGE CPP, FlexibleInstances #-}
module Main where

import System.Exit
import System.IO

import Control.Monad.Reader
import Control.Exception

#ifdef USE_HTTP
import Network.HTTP (rspBody)
import Network.HTTP.UserAgent as UA
#endif

import System.Environment (getArgs, getProgName)
import System.Console.GetOpt

import Text.Printf

import qualified Data.ByteString.Lazy as L (ByteString, concat, hGetContents, hPut)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Char
import Data.List hiding (sort)

import System.Locale (defaultTimeLocale)
import Data.Time.Format (formatTime)
import Data.Time.Clock (getCurrentTime)

import Codec.Container.Ogg.Chain
import Codec.Container.Ogg.Chop
import Codec.Container.Ogg.ContentType
import Codec.Container.Ogg.Page
import Codec.Container.Ogg.Packet
import Codec.Container.Ogg.RawPage
import Codec.Container.Ogg.Sort
import Codec.Container.Ogg.Timestamp
import Codec.Container.Ogg.Track

------------------------------------------------------------
-- Version
--

hoggVersion :: String
hoggVersion = "0.4.0"

showVersion :: IO ()
showVersion = do
    putStrLn $ "hogg version " ++ hoggVersion
    exitWith ExitSuccess

hoggDesc :: String
hoggDesc = para [
    "hogg is a commandline tool for manipulating Ogg files. It supports " ++
    "chained and multiplexed files conformant with RFC3533. Hogg can parse " ++
    "headers for " ++ interpretedCodecs ++ ", and can " ++
    "read and write Ogg Skeleton logical bitstreams."]

hoggAuthors :: String
hoggAuthors = "Conrad Parker"

------------------------------------------------------------
-- Content types for which granulepos is interpreted
--

interpretedCodecs :: String
interpretedCodecs = englishList $ filter (/= "Skeleton") knownContentTypes

englishList :: [String] -> String
englishList [] = []
englishList [a,b] = a ++ " and " ++ b
englishList (a:as) = a ++ ", " ++ englishList as

------------------------------------------------------------
-- Paragraph rendering
--

para :: [String] -> String
para ss = concat $ intersperse "\n" (map (\s -> breakLines 76 s) ss)

indent :: Int -> String -> String
indent i s = unlines $ map (\x -> indentation ++ x) (lines s)
  where
    indentation = take i $ repeat ' '

-- breakLines leftIndent columnWidth text
breakLines :: Int -> String -> String
breakLines n s
  | length s < n = s ++ "\n"
  | otherwise    = line' ++ "\n" ++ breakLines n rest'
  where
    (line, rest) = splitAt n s
    (rSpill, rLine) = break isSpace (reverse line)
    line' = reverse rLine
    rest' = reverse rSpill ++ rest


------------------------------------------------------------
--  HOggTool datatype
--

data HOggTool =
  HOggTool {
    hotConfig :: Config,
    hotFilenames :: [String]
  }

type Hot = ReaderT HOggTool IO

------------------------------------------------------------
-- Subcommands
--

data SubCommand =
  SubCommand {
    subName :: String,
    subMethod :: Hot (),
    subCategory :: String,
    subSynopsis :: String,
    subDescription :: String,
    subExamples :: [(String,String)], -- [(example description, flags)]
    subOptions :: [[OptDescr Option]] -- eg. [rangeOptions, cTypeOptions]
  }

subCommands :: [SubCommand]
subCommands = [
               infoSub,
               dumpPacketsSub,
               dumpPagesSub,
               dumpRawPagesSub,
               rewritePagesSub,
               rewritePacketsSub,
               chopSub,
               mergePagesSub,
               sortPagesSub,
               addSkelSub,
               countPacketsSub,
               countrwPagesSub,
               countPagesSub,
               knownCodecsSub,
               selfCheckSub,
               helpSub,
               manSub
              ]

------------------------------------------------------------
-- Options processing
--

data Config =
  Config {
    contentTypeCfg :: Maybe ContentType,
    noSkelCfg :: Bool,
    outputCfg :: Maybe String,
    startCfg :: Maybe Timestamp,
    endCfg :: Maybe Timestamp,
    files :: [FilePath]
  }

dftConfig :: Config
dftConfig =
  Config {
    contentTypeCfg = Nothing,
    noSkelCfg = False,
    outputCfg = Nothing,
    startCfg = Nothing,
    endCfg = Nothing,
    files = ["-"]
  }

-- We will be comparing preset lists of these explicitly, so define an Eq
-- for options. We just compare short options for simplicity.
instance Eq (OptDescr Option) where
  (==) (Option a _ _ _) (Option b _ _ _) =  a == b

-- Options available for subcommands
--
data Option = Help
            | Version
            | ContentTypeOpt String
            | SkelOpt
            | StartOpt String
            | EndOpt String
            | OutputOpt String
            deriving Eq

options :: [OptDescr Option]
options = concat $ miscOptions : allOptions

allOptions :: [[OptDescr Option]]
allOptions = [cTypeOptions, skelOptions, rangeOptions, outputOptions]

miscOptions, cTypeOptions, skelOptions, rangeOptions, outputOptions :: [OptDescr Option]

miscOptions = [
  Option ['h', '?'] ["help"] (NoArg Help) "Display this help and exit",
  Option ['V'] ["version"] (NoArg Version)
         "Output version information and exit" ]

cTypeOptions = [
  Option ['c']      ["content-type"] (ReqArg ContentTypeOpt "Content-Type")
         "Select the logical bitstreams for a specified content type" ]

skelOptions = [
  Option ['k']      ["no-skeleton"] (NoArg SkelOpt)
         "Do NOT include a Skeleton bitstream in the output" ]

rangeOptions = [
  Option ['s']      ["start"] (ReqArg StartOpt "Timestamp")
         "Specify a start time",
  Option ['e']      ["end"] (ReqArg EndOpt "Timestamp")
           "Specify an end time" ]

outputOptions = [
  Option ['o']      ["output"] (ReqArg OutputOpt "filename")
         "Specify output filename" ]

processArgs :: [String] -> IO (Config, [String])
processArgs args = do
  case getOpt RequireOrder options args of
    (opts, args'  , []  ) -> do
                        processHelp opts
                        config <- processConfig dftConfig opts
                        return (config, args')
    (_, _, _ : _) -> return (dftConfig, args)

processHelp :: [Option] -> IO ()
processHelp opts = do
  name <- getProgName
  let header = "\nUsage: " ++ name ++ " [options] filename\n"
  when (Help `elem` opts) $ do
    putStrLn $ usageInfo header options
    exitWith ExitSuccess
  return ()

processConfig :: Config -> [Option] -> IO Config
processConfig = foldM processOneOption
  where
    processOneOption config (ContentTypeOpt ctype) = do
      -- let c = catchRead "Invalid content type" ctype
      let c = parseType ctype
      return $ config {contentTypeCfg = c}
    processOneOption config (SkelOpt) = do
      return $ config {noSkelCfg = True}
    processOneOption config (OutputOpt output) = do
      return $ config {outputCfg = Just output}
    processOneOption config (StartOpt start) = do
      let s = catchRead "Invalid start time" start
      return $ config {startCfg = Just s}
    processOneOption config (EndOpt end) = do
      let e = catchRead "Invalid end time" end
      return $ config {endCfg = Just e}
    processOneOption config _ = return config

catchRead :: (Read a) => String -> String -> a
catchRead msg x = case reads x of
    [(ms,_mt)] -> ms
    _ -> error (msg ++ ": " ++ x) -- actually [] on error, but catch _ anyway

------------------------------------------------------------
-- Hot: actions for getting Ogg data from the input files
--

-- get the named contents as a bytestring
open :: String -> IO L.ByteString
open f =
#ifdef USE_HTTP
  case (isPrefixOf "http://" f) of
    True -> do
      rsp <- UA.get f
      return $ rspBody rsp
    False ->
#endif
             do
      h <- openFile f ReadMode
      i <- L.hGetContents h
      return i

-- rawpages is used only by "hogg dumpraw"
rawpages :: Hot [[OggRawPage]]
rawpages = do
    filenames <- asks hotFilenames
    inputs <- mapM (liftIO . open) filenames
    return $ map rawPageScan inputs

-- chains, tracks, pages, packets
allChains :: Hot [[OggChain]]
allChains = do
    filenames <- asks hotFilenames
    inputs <- mapM (liftIO . open) filenames
    return $ map chainScan inputs

-- All tracks, from all files, matching the given criteria
tracks :: Hot [[[OggTrack]]]
tracks = chainMatch chainTracks

-- All pages, from all files, matching the given criteria
pages :: Hot [[[OggPage]]]
pages = chainMatch chainPages

-- All packets, from all files, matching the given criteria
packets :: Hot [[[OggPacket]]]
packets = chainMatch chainPackets

-- All chains, from all files, with elements matching the given criteria
chains :: Hot [[OggChain]]
chains = chainMatchM chainFilter

-- | A generic function to run a Hot function over all chains
chainMatchM :: (OggChain -> Hot a) -> Hot [[a]]
chainMatchM f = do
    c <- allChains
    let a = map (mapM f) c
    sequence a

-- | Filter all elements of a chain by the given criteria
chainFilter :: OggChain -> Hot OggChain
chainFilter (OggChain ts gs ps) = do
    ts' <- mType ts ts
    gs' <- mType ts gs
    ps' <- mType ts ps
    return $ OggChain ts' gs' ps'

-- | A generic function to pull a list of things from a chain
chainMatch :: (OggChain -> a) -> Hot [[a]]
chainMatch f = do
    c <- chains
    return $ sequence $ map (map f) c

-- | Filter a ContentTyped list by the given content type
mType :: (ContentTypeImplied a) => [OggTrack] -> [a] -> Hot [a]
mType tks xs = do
    config <- asks hotConfig
    return $ case (contentTypeCfg config) of
      Nothing -> xs
      Just t -> case (noSkelCfg config) of
                  False -> filter (contentTypeImplies tks t) xs
                  True  -> filter (contentTypeIs t) xs

-- | Apply matchRange to all the inner inner lists
matchRange :: (Timestampable a) => [[[a]]] -> Hot [[[a]]]
matchRange as = sequence $ (map (mapM mRange)) as

-- | Filter a Timestampable list by the given time range
mRange :: (Timestampable a) => [a] -> Hot [a]
mRange xs = do
    config <- asks hotConfig
    return $ between (startCfg config) (endCfg config) xs

------------------------------------------------------------
-- Output helpers
--

-- The output handle; stdout unless otherwise specified
outputHandle :: Config -> IO Handle
outputHandle config =
    maybe (evaluate stdout) (\f -> openBinaryFile f WriteMode) (outputCfg config)

-- Output a Data.ByteString.Lazy.Char8
outputC :: C.ByteString -> Hot ()
outputC bs = do
    config <- asks hotConfig
    h <- liftIO $ outputHandle config
    liftIO $ C.hPut h bs
    liftIO $ hClose h

-- Output a Data.ByteString.Lazy
outputL ::  L.ByteString -> Hot ()
outputL bs = do
    config <- asks hotConfig
    h <- liftIO $ outputHandle config
    liftIO $ L.hPut h bs
    liftIO $ hClose h

-- Output with a text banner per input file
reportPerFile :: [C.ByteString] -> Hot ()
reportPerFile [r] = outputC r -- Don't add banners if only one file to report
reportPerFile l = do
    filenames <- asks hotFilenames
    let fHeader f = C.pack $ printf "Filename: %s\n\n" f
    let fText = zip (map fHeader filenames) l
    let sep = C.pack $ replicate 60 '-' ++ "\n"
    let banner (f,t) = C.concat [sep,f,t]
    outputC $ C.concat $ map banner fText

-- Output binary data
outputPerFile :: [L.ByteString] -> Hot ()
outputPerFile l = outputL $ L.concat l

-- Place a marker betwen the reports for each chain
reportPerChain :: [C.ByteString] -> C.ByteString
reportPerChain l = C.concat $ intersperse (C.pack (chainMarker++"\n\n")) l
  where
    -- chainMarker = "><> New Chain <><><><"
    chainMarker = "><><><> New Chain ><>"

-- Concat the output for each chain
outputPerChain :: [L.ByteString] -> L.ByteString
outputPerChain = L.concat

------------------------------------------------------------
-- info
--

infoSub :: SubCommand
infoSub = SubCommand "info" info
    "Reporting" "Display information about the specified files and their bitstreams"
    infoDesc
    [("Describe all bitstreams in file.ogg", "file.ogg"),
     ("Describe only the Theora bitstream in file.ogv", "-c theora file.ogv")]
    [cTypeOptions]

infoDesc :: String
infoDesc = para [
  "Chain handling: Information for each chain is reported separately."]

info :: Hot ()
info = do
    matchTracks <- tracks
    let t = \x -> show x ++ "\n" -- Add a newline after each track's info
    let i = \x -> reportPerChain $ map (C.concat . map (C.pack . t)) x
    reportPerFile $ map i matchTracks

------------------------------------------------------------
-- dumpPackets (dump)
--

dumpPacketsSub :: SubCommand
dumpPacketsSub = SubCommand "dump" dumpPackets
    "Reporting" "Hexdump packets of an Ogg file"
    dumpDesc
    [("Dump all bitstreams in file.ogg", "file.ogg"),
     ("Dump only the Theora bitstream in file.ogv", "-c theora file.ogv")]
    allOptions

dumpDesc :: String
dumpDesc = para [
  "Chain handling: Each chain is dumped separately."]

dumpPackets :: Hot ()
dumpPackets = do
    matchPackets <- matchRange =<< packets
    let d = \x -> reportPerChain $ map (C.concat . map packetToBS) x
    reportPerFile $ map d matchPackets

------------------------------------------------------------
-- countPackets (packetcount)
--

countPacketsSub :: SubCommand
countPacketsSub = SubCommand "packetcount" countPackets
    "Testing" "Count packets of an Ogg file"
    ""
    [("Count packets of all bitstreams in file.ogg", "file.ogg"),
     ("Count packets from only the Theora bitstreams in file.ogv",
      "-c theora file.ogv")]
    [cTypeOptions, rangeOptions]

countPackets :: Hot ()
countPackets = do
    matchPackets <- matchRange =<< packets
    let c = \x -> C.pack $ show (length x) ++ " packets\n"
    let r = \x -> reportPerChain $ map c x
    reportPerFile $ map r matchPackets

------------------------------------------------------------
-- rewritePages (rip)
--

rewritePagesSub :: SubCommand
rewritePagesSub = SubCommand "rip" rewritePages
    "Extraction" "Rip selected logical bistreams from an Ogg file (default: all)"
    rewriteDesc
    [("Extract all bitstreams from file.ogg", "-o output.ogg file.ogg"),
     ("Extract only the Theora bitstream from file.ogv",
      "-c theora -o output.ogv file.ogv")]
    allOptions

rewriteDesc :: String
rewriteDesc = para [
  "Skeleton handling: If the input file has a skeleton track, then its " ++
  "metadata describing the bitstream being ripped will be replicated.",
  "Chain handling: All matching bitstreams from all chains are ripped, and " ++
  "chain ordering is preserved."]

rewritePages :: Hot ()
rewritePages = do
    matchPages <- pages
    let r = \x -> L.concat $ map pageWrite x
    let r2 = \x -> outputPerChain $ map r x
    outputPerFile $ map r2 matchPages

------------------------------------------------------------
-- rewritePackets (reconstruct)
--

rewritePacketsSub :: SubCommand
rewritePacketsSub = SubCommand "reconstruct" rewritePackets
    "Extraction" "Reconstruct an Ogg file by doing a full packet demux"
    rewriteDesc -- reuse info from rip
    [("Reconstruct all bitstreams from file.ogg", "-o output.ogg file.ogg"),
     ("Reconstruct only the Theora bitstream from file.ogv",
      "-c theora -o output.ogv file.ogv")]
    allOptions

rewritePackets :: Hot ()
rewritePackets = do
    matchPackets <- packets
    let r = \x -> L.concat $ map pageWrite (packetsToPages x)
    let r2 = \x -> outputPerChain $ map r x
    outputPerFile $ map r2 matchPackets

------------------------------------------------------------
-- chop
--

chopSub :: SubCommand
chopSub = SubCommand "chop" chopPages
    "Editing" "Extract a section (specify start and/or end time)"
    chopDesc
    [("Extract the first minute of file.ogx", "-e 1:00 file.ogx"),
     ("Extract from the second to the fifth minute of file.ogx",
      "-s 2:00 -e 5:00 -o output.ogx file.ogx"),
     ("Extract only the Theora video stream, from 02:00 to 05:00, of file.ogv",
      "-c theora -s 2:00 -e 5:00 -o output.ogv file.ogv"),
     ("Extract, specifying SMPTE-25 frame offsets",
      "-c theora -s smpte-25:00:02:03::12 -e smpte-25:00:05:02::04 -o output.ogv file.ogv")]
    allOptions

chopDesc :: String
chopDesc = para [
    "This chops a section of an Ogg file. " ++
    "It correctly interprets the granulepos timestamps of " ++
    interpretedCodecs ++ " bitstreams.",

    "The output file contains copies of the headers of the input file, and " ++
    "all the codec data required to correctly decode the content between " ++
    "the start and end times specified on the commandline. For codecs with " ++
    "data dependencies like video keyframes, the keyframe prior to the " ++
    "starting time will be included in the output.",

    "Note that 'hogg chop' operates by copying pages of Ogg data; it does " ++
    "not strip partial packets from the first or last data page included in " ++
    "the output. It does however ensure to set the end of stream flag on " ++
    "the last page of each logical bitstream.",

    "Skeleton handling: By default, the output will " ++
    "contain a Skeleton track specifying the start of the chop as " ++
    "presentation time.",

    "Chain handling: The chop range is applied to each chain, and chain " ++
    "ordering is preserved."]

chopPages :: Hot ()
chopPages = do
    config <- asks hotConfig
    matchChains <- chains
    chopChains <- mapM (mapM (chopRange config)) matchChains
    let c = \x -> L.concat $ map pageWrite (chainPages x)
    let c2 = \x -> outputPerChain $ map c x
    outputPerFile $ map c2 chopChains

chopRange :: Config -> OggChain -> Hot OggChain
chopRange (Config _ noSkel _ start end _) xs = case noSkel of
  False -> liftIO $ chopWithSkel start end xs
  True  -> liftIO $ chop start end xs

------------------------------------------------------------
-- addSkel (addskel)
--

addSkelSub :: SubCommand
addSkelSub = SubCommand "addskel" addSkel
    "Editing" "Write a Skeleton logical bitstream"
    addSkelDesc
    [("Add a Skeleton to file.ogg", "-o output.oga file.ogg")]
    [outputOptions]

addSkelDesc :: String
addSkelDesc = para [
  "Chain handling: A Skeleton track is added to each chain."]

addSkel :: Hot ()
addSkel = do
    c <- allChains
    skels <- mapM (mapM ioAddSkeleton) c
    let s = \x -> L.concat $ map pageWrite (chainPages x)
    let s2 = \x -> outputPerChain $ map s x
    outputPerFile $ map s2 skels
  where
    ioAddSkeleton x = liftIO $ chainAddSkeleton x

------------------------------------------------------------
-- countrwPages (countrw)
--

countrwPagesSub :: SubCommand
countrwPagesSub = SubCommand "countrw" countrwPages
    "Testing" "Rewrite via packets and display a count of pages produced"
    ""
    [("Rewrite and count packets of all bitstreams in file.ogg", "file.ogg"),
     ("Rewrite and count packets from only the Theora bitstream in file.ogv",
      "-c theora file.ogv")]
    [cTypeOptions, rangeOptions]

countrwPages :: Hot ()
countrwPages = do
    matchPages <- pages
    let c = \x -> C.pack $ printf "%d pages\n" (length (packetsToPages (pagesToPackets x)))
    let r = \x -> reportPerChain $ map c x
    reportPerFile $ map r matchPages

------------------------------------------------------------
-- countPages (pagecount)
--

countPagesSub :: SubCommand
countPagesSub = SubCommand "pagecount" countPages
    "Testing" "Count pages of an Ogg file"
    ""
    [("Count pages of all bitstreams in file.ogg", "file.ogg"),
     ("Count pages from only the Theora bitstream in file.ogv",
      "-c theora file.ogv")]
    [cTypeOptions, rangeOptions]

countPages :: Hot ()
countPages = do
    matchPages <- matchRange =<< pages
    let c = \x -> C.pack $ printf "%d pages\n" (length x)
    let r = \x -> reportPerChain $ map c x
    reportPerFile $ map r matchPages

------------------------------------------------------------
-- dumpPages (pagedump)
--

dumpPagesSub :: SubCommand
dumpPagesSub = SubCommand "pagedump" dumpPages
    "Reporting" "Display page structure of an Ogg file"
    dumpDesc -- reuse info from dumpPackets
    [("Dump pages of all bitstreams in file.ogg", "file.ogg"),
     ("Dump pages of only the Theora bitstream in file.ogv",
      "-c theora file.ogv")]
    allOptions

dumpPages :: Hot ()
dumpPages = do
    matchPages <- matchRange =<< pages
    let d = \x -> C.concat $ map (C.pack . show) x
    let r = \x -> reportPerChain $ map d x
    reportPerFile $ map r matchPages

------------------------------------------------------------
-- mergePages (merge)
--

mergePagesSub :: SubCommand
mergePagesSub = SubCommand "merge" mergePages
    "Editing" "Merge, interleaving pages in order of presentation time"
    mergeDesc
    [("Merge pages of audio.oga and video.ogv",
      "-o output.ogv audio.oga video.ogv")]
    [outputOptions]

mergeDesc :: String
mergeDesc = para [
    "This merges Ogg files together, interleaving pages in order of " ++
    "presentation time. It correctly interprets the granulepos timestamps " ++
    "of " ++ interpretedCodecs ++ "bitstreams. ",

    "For example, if you have an Ogg Theora video file, and its " ++
    "soundtrack stored separately as an Ogg Speex audio file, and you can " ++
    "use 'hogg merge' to create a single Ogg file containing the video and " ++
    "audio, interleaved together in parallel. ",

    "Similarly, using 'hogg merge' on a collection of Ogg Vorbis audio " ++
    "files will create a big Ogg file with all the songs in parallel, ie. " ++
    "interleaved for simultaneous playback. Such a file is proper Ogg, but " ++
    "not \"Ogg Vorbis I\" -- the Ogg Vorbis I specification defines an " ++
    "Ogg Vorbis file as an Ogg file containing only one Vorbis track at a " ++
    "time (ie. no parallel multiplexing). Many music players (which use " ++
    "libvorbisfile) aren't designed to play multitrack Ogg files. In " ++
    "general however, video players, and anything built on a multimedia " ++
    "framework (like GStreamer, DirectShow etc.) will probably be able to " ++
    "handle such files. ",

    "If you want to create a file containing some Ogg files sequenced one " ++
    "after another, then you should simply concatenate them together using " ++
    "cat. In Ogg this is called \"chaining\". If you cat Ogg Vorbis I audio " ++
    "files together, then the result will also be a compliant Ogg Vorbis " ++
    "file.",

    "Theora handling: Theora BOS pages will be written before the BOS pages " ++    "of other codecs.",

    "Skeleton handling: If any of the input files contain a Skeleton track " ++
    "then the output file will contain exactly one Skeleton track. Its " ++
    "Skeleton BOS (fishead) page will be copied from the first input file " ++
    "with Skeleton. All fisbone packets present in input files will be " ++
    "copied into the output file.",

    "Chain handling: Only the first chain of each input file is merged."]

mergePages :: Hot ()
mergePages = do
    matchPages <- pages
    -- XXX: only use the first chain of each input file. Using subsequent
    -- chains won't work anyway unless corresponding chains in each file are
    -- of identical duration.
    let firstChainPages = map head matchPages
    outputL $ L.concat $ map pageWrite $ merge firstChainPages

------------------------------------------------------------
-- sortPages (sort)
--

sortPagesSub :: SubCommand
sortPagesSub = SubCommand "sort" sortPages
    "Editing" "Rewrite with correct page ordering"
    sortDesc
    [("Correct the page ordering in broken.ogv", "-o fixed.ogv broken.ogv")]
    [outputOptions]

sortDesc :: String
sortDesc = para [
    "This sorts an Ogg file, interleaving pages in order of presentation " ++
    "time. It correctly interprets the granulepos timestamps of " ++
    interpretedCodecs ++ " bitstreams. ",
    "Some encoders produce files with incorrect page ordering; for example, " ++
    "some audio and video pages may occur out of order. Although these " ++
    "files are usually playable, it can be difficult to accurately seek or " ++
    "scrub on them, increasing the likelihood of glitches during playback. " ++
    "Players may also need to use more memory in order to buffer the audio " ++
    "and video data for synchronized playback, which can be a problem when " ++
    "the files are viewed on low-memory devices.",

    "Theora handling: Theora BOS pages will be written before BOS pages of " ++
    "other codecs.",

    "Skeleton handling: If a Skeleton track is present in the input file, " ++
    "the first page of the output file will be the Skeleton BOS page, and " ++
    "the order of other Skeleton packets is preserved.",

    "Chain handling: Each chain is sorted separately, and chain ordering is " ++
    "preserved."]

sortPages :: Hot ()
sortPages = do
    matchPages <- pages
    let r = \x -> L.concat $ map pageWrite $ sort x
    let r2 = \x -> outputPerChain $ map r x
    outputPerFile $ map r2 matchPages

------------------------------------------------------------
-- dumpRawPages (dumpraw)
--

dumpRawPagesSub :: SubCommand
dumpRawPagesSub = SubCommand "dumpraw" dumpRawPages
    "Reporting" "Dump raw (unparsed) page data"
    dumpRawDesc
    [("Dump raw pages of all bitstreams in file.ogg", "file.ogg"),
     ("Dump raw pages of only the Theora bitstream in file.ogv",
      "-c theora file.ogv")]
    allOptions

dumpRawDesc :: String
dumpRawDesc = para [
  "This operates below the level of Content-Type, Skeleton and chain " ++
  "handling, and simply dumps raw page data for debugging purposes."]

dumpRawPages :: Hot ()
dumpRawPages = do
    matchPages <- rawpages
    reportPerFile $ map (C.pack . show) matchPages

------------------------------------------------------------
-- known-types
--

knownCodecsSub :: SubCommand
knownCodecsSub = SubCommand "known-codecs" knownCodecs
    "Miscellaneous" "List codecs known by this version of hogg"
    knownCodecsDesc
    [] -- Examples
    [] -- Options

knownCodecsDesc :: String
knownCodecsDesc = para [
    "All hogg subcommands can interpret the timestamps of " ++
    interpretedCodecs ++ " bitstreams.",
    "Running 'hogg known-codecs' will simply list these codec names, " ++
    "one per line. This format is designed to be easily machine readable."]

knownCodecs :: Hot ()
knownCodecs = liftIO $ mapM_ putStrLn knownContentTypes

------------------------------------------------------------
-- selfcheck
--

selfCheckSub :: SubCommand
selfCheckSub = SubCommand "selfcheck" selfCheck
    "Testing" "Check consistency of help example options"
    ""
    [] -- Examples
    [] -- Options

selfCheck :: Hot ()
selfCheck = liftIO $ mapM_ checkArgs allExamples
  where allExamples = concatMap cmd subCommands
        cmd s = map (\e -> unwords ["hogg", subName s, snd e]) (subExamples s)

checkArgs :: String -> IO ()
checkArgs line = do
  let (_:_:args) = words line
  case getOpt RequireOrder options args of
    (_, args'  , []  ) -> do
        -- Allow non options which are either names of subcommands (as
        -- examples for "hogg help"), or filenames (ending in .ogg)
        let a = filter (flip notElem (map subName subCommands)) $
                filter (not . validSuffix) args'
        case a of
          [] -> return ()
          _  -> report (unwords ("non option":(map (\x -> '`':x++['\'']) a)))
    (_, _, errs) -> mapM_ report (map (reverse.tail.reverse) errs)
  where
    report msg = hPutStrLn stderr $ "Warning: " ++ msg ++ " in help example:\n  " ++ line
    validSuffix s = or $ map (flip isSuffixOf s) validExtensions
    validExtensions = [".ogg", ".oga", ".ogv", ".ogx", ".spx"]

------------------------------------------------------------
-- help
--

helpSub :: SubCommand
helpSub = SubCommand "help" help
    "Commands" "Display help for a specific subcommand (eg. \"hogg help chop\")"
    ""
    [("Display help for the \"hogg chop\" subcommand", "chop")]
    [] -- Options

help :: Hot ()
help = do
    args <- asks hotFilenames
    outputC $ C.concat $ map C.pack $ longHelp args
    selfCheck

longHelp :: [String] -> [String]
-- | "hogg help" with no arguments: Give a list of all subcommands
longHelp [] =
    ["Usage: hogg <subcommand> [options] filename ...\n\n"] ++
    [indent 2 hoggDesc, "\n"] ++
    map categoryHelp ["Commands", "Reporting", "Extraction", "Editing", "Miscellaneous"] ++
    -- map categoryHelp ["Testing"] ++
    ["Please report bugs to <ogg-dev@xiph.org>\n"]

-- | "hogg help command": Give command-specific help
longHelp (command:_) = contextHelp command m
  where m = filter (\x -> subName x == command) subCommands

-- | Provide synopses for a specific category of commands
categoryHelp :: String -> String
categoryHelp c = c ++ ":\n" ++ concat (map itemHelp items) ++ "\n"
  where items = filter (\x -> subCategory x == c) subCommands
        itemHelp i = printf "  %-14s%s\n" (subName i) (subSynopsis i)

-- | Provide detailed help for a specific command
contextHelp :: [Char] -> [SubCommand] -> [String]
contextHelp command [] = longHelp [] ++ contextError
  where contextError = ["\n*** \"" ++ command ++ "\": Unknown command.\n"]
contextHelp command (item:_) = synopsis ++ usage ++ description ++ examples ++
    ["\n" ++ optionsHelp item]
  where usage = ["Usage: hogg " ++ command ++ hasOpts command ++ outOpts]
        hasOpts "help" = " <subcommand>"
        hasOpts _ = " [options]"
        outOpts = case elem outputOptions (subOptions item) of
                    True -> " filename ...\n"
                    False -> "\n"
        synopsis = [command ++ ": " ++ subSynopsis item ++ "\n"]
        description = case (subDescription item) of
                    "" -> []
                    _  -> ["\n" ++ indent 2 (subDescription item)]
        examples = case (subExamples item) of
                     [] -> []
                     _  -> ["\nExamples:"] ++
                           flip map (subExamples item) (\(desc,opts) ->
                             "\n  " ++ desc ++ ":\n    hogg " ++ command ++
                             " " ++ opts ++ "\n")

-- | Provide usage information [for a specific command]
optionsHelp :: SubCommand -> String
optionsHelp item = usageInfo "Options:"
                     (concat $ miscOptions : subOptions item)

------------------------------------------------------------
-- man
--

manSub :: SubCommand
manSub = SubCommand "man" man
    "Commands" "Generate Unix man page for a specific subcommand (eg. \"hogg man chop\")"
    ""
    [("Generate a man page for the \"hogg chop\" subcommand", "chop")]
    [] -- Options

man :: Hot ()
man = do
    args <- asks hotFilenames
    -- let dateStamp = "1 June 2007"
    currentTime <- liftIO getCurrentTime
    let dateStamp = formatTime defaultTimeLocale "%B %Y" currentTime
    outputC $ C.concat $ map C.pack $ (longMan dateStamp) args
    selfCheck

headerMan :: String -> [String]
headerMan dateStamp = [".TH HOGG 1 \"", dateStamp, "\" \"hogg\" \"Annodex\"\n"]

synopsisMan :: String -> [SubCommand] -> [String]
synopsisMan _ [] =
    [".SH SYNOPSIS\n\n.B hogg\n.RI SUBCOMMAND\n[\n.I OPTIONS\n]\n.I filename ...\n\n"]
synopsisMan command (item:_) =
    [".SH SYNOPSIS\n\n.B hogg\n.RI ", command, "\n", hasOpts command, outOpts, "\n"]
  where hasOpts "help" = ".I <subcommand>\n"
        hasOpts _ = "[\n.I OPTIONS\n]\n"
        outOpts = case elem outputOptions (subOptions item) of
                    True -> ".I filename ...\n"
                    False -> "\n"

authorsMan :: String -> [String]
authorsMan command =
    [".SH AUTHORS\n\nhogg was written by ", hoggAuthors, "\n\n"] ++
    ["This manual page was autogenerated by\n.B hogg man" ++ space command ++ ".\n\n"] ++
    ["Please report bugs to <ogg-dev@xiph.org>\n"]
  where
    space "" = ""
    space c = ' ':c

descMan :: String -> [String]
descMan desc = [".SH DESCRIPTION\n", desc, "\n"]

longMan :: String -> [String] -> [String]
-- | "hogg man" with no arguments: Give a list of all subcommands
longMan dateStamp [] =
    headerMan dateStamp ++
    [".SH NAME\n"] ++
    ["hogg \\- inspect or manipulate Ogg multimedia files\n\n"] ++
    synopsisMan "SUBCOMMAND" [] ++
    descMan (".B hogg\n" ++ (drop 5 hoggDesc)) ++
    map categoryMan ["Commands", "Reporting", "Extraction", "Editing", "Miscellaneous"] ++
    -- map categoryMan ["Testing"] ++
    authorsMan "" ++
    [".SH \"SEE ALSO\"\n\n"] ++
    [".PP\n"] ++ map (\x -> "\\fB"++x++"\\fR(1)\n") seeAlso
  where
    seeAlso = ["ogginfo", "oggzinfo", "oggzrip", "oggzmerge", "oggzdump", "oggzdiff"]
    

-- | "hogg help command": Give command-specific help
longMan dateStamp (command:_) = contextMan dateStamp command m
  where m = filter (\x -> subName x == command) subCommands

-- | Provide synopses for a specific category of commands
categoryMan :: String -> String
categoryMan c = ".SH " ++ (map toUpper c) ++ "\n" ++ concat (map itemMan items) ++ "\n"
  where items = filter (\x -> subCategory x == c) subCommands
        itemMan i = printf ".IP %s\n%s\n" (subName i) (subSynopsis i)

-- | Provide detailed help for a specific command
contextMan :: String -> [Char] -> [SubCommand] -> [String]
contextMan dateStamp _ [] = longMan dateStamp []
contextMan dateStamp command i@(item:_) =
    headerMan dateStamp ++
    synopsisMan command i ++
    synDesc ++ description ++
    ["\n" ++ optionsMan item] ++
    examples ++
    authorsMan command
  where synDesc = descMan (subSynopsis item)
        -- desc = [command ++ ": " ++ subSynopsis item ++ "\n"]
        description = case (subDescription item) of
                    "" -> []
                    _  -> ["\n" ++ (subDescription item)]
        examples = case (subExamples item) of
                     [] -> []
                     _  -> ["\n.SH EXAMPLES\n"] ++
                           flip map (subExamples item) (\(desc,opts) ->
                             ".PP\n" ++ desc ++ ":\n.PP\n.RS\n\\f(CWhogg " ++ command ++
                             " " ++ opts ++ "\\fP\n.RE\n")

-- | Provide usage information [for a specific command]
optionsMan :: SubCommand -> String
optionsMan item = usageInfo ".SH OPTIONS"
                     (concat $ miscOptions : subOptions item)

------------------------------------------------------------
-- main
--

helpStrings :: [[Char]]
helpStrings = ["--help", "-h", "-?"]

versionStrings :: [[Char]]
versionStrings = ["--version", "-V"]

isHelp :: String -> Bool
isHelp x = elem x helpStrings

isVersion :: String -> Bool
isVersion x = elem x versionStrings

initTool :: [String] -> IO HOggTool
initTool args = do
    (config, filenames) <- processArgs args
    return $ HOggTool config filenames

main :: IO ()
main = do
    allArgs <- getArgs
    when (any isHelp allArgs) $ showHelp allArgs
    when (any isVersion allArgs) $ showVersion
    handleSubCommand allArgs

showHelp :: [String] -> IO ()
showHelp allArgs = -- bracket init1 finish1 loop1
  init1 >>= loop1
  where
    init1 = initTool $ filter (not . isHelp) allArgs
    loop1 st = runReaderT run1 st
    run1 = help
    -- finish1 = exitWith ExitSuccess

handleSubCommand :: [String] -> IO ()
handleSubCommand [] = -- bracket (initTool []) finish loop0
  (initTool []) >>= loop0
  where
    -- finish = exitWith ExitSuccess
    loop0 st = runReaderT help st

handleSubCommand (command:args) = -- bracket (initTool args) finish loop1
  (initTool args) >>= loop1
  where
    -- finish = exitWith ExitSuccess
    loop1 st = runReaderT run st
    run = act $ filter (\x -> subName x == command) subCommands
    act [] = help
    act (s:_) = (subMethod s)
