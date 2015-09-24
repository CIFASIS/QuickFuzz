module Main where

import qualified Bmp
import qualified Gif
import qualified Ogg
import qualified Tga
import qualified Zip
import qualified Tar
import qualified Jpeg
import qualified Tiff
import qualified Xml
import qualified Html
import qualified Png
import qualified Pnm
import qualified Gzip
import qualified Bzip
import qualified Js
import qualified Svg

import System.Console.ArgParser

data MainArgs = MainArgs String String String String Int Int
                                    deriving(Show)

parser :: ParserSpec MainArgs
parser = MainArgs
    `parsedBy` reqPos          "type"        `Descr` "File Type to generate (e.g. Bmp, Ogg, Gif, ...)"
    `andBy`    reqPos          "name"        `Descr` "Output filename"
    `andBy`    reqPos          "command"     `Descr` "Full command line to execute"
    `andBy`    optFlag "fuzz"  "action"      `Descr` "Action to execute (fuzz | check | gen)"
    `andBy`    optFlag 100000000 "max-success" `Descr` "Number of attempts"
    `andBy`    optFlag 20      "max-size"    `Descr` "Maximum size in bytes of generated values"

cli :: IO (CmdLnInterface MainArgs)
cli =
    (`setAppDescr` "An experimental grammar fuzzer in Haskell using QuickCheck, Template Haskell and specific libraries from Hackage. It found interesting bugs in gdk-pixbuf, jasper and unzip.")
    <$> (`setAppEpilog` "Enjoy!")
    <$> mkApp parser

dispatch :: MainArgs -> IO ()
dispatch (MainArgs t n c a succ size) = case t of
        "Bmp"  -> Bmp.main n c a succ size
        "Gif"  -> Gif.main n c a succ size
        "Ogg"  -> Ogg.main n c a succ size
        "Zip"  -> Zip.main n c a succ size
        "Bzip"  -> Bzip.main n c a succ size
        "Tar"  -> Tar.main n c a succ size
        "Tga"  -> Tga.main n c a succ size
        "Jpeg" -> Jpeg.main n c a succ size
        "Tiff" -> Tiff.main n c a succ size
        "Xml"  -> Xml.main n c a succ size
        "Html"  -> Html.main n c a succ size
        "Js"  -> Js.main n c a succ size
        "Png"  -> Png.main n c a succ size
        "Pnm"  -> Pnm.main n c a succ size
        "Svg"  -> Svg.main n c a succ size

main = do
    interface <- cli
    runApp interface dispatch
