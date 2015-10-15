module Main where

import qualified Zip
import qualified Bmp
import qualified Gif
import qualified Ogg
import qualified Tga
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
import qualified SimpleSvg
--import qualified Svg
import qualified Dot
import qualified ByteString

import System.Console.ArgParser 
import Args
import System.Directory 
import System.Exit

dispatch :: MainArgs -> IO ()
dispatch args = safetyCheck args >>
        case (findFileType args) of

        "Bmp"  -> Bmp.main args
        "Dot"  -> Dot.main args
        "Gif"  -> Gif.main args
        "Ogg"  -> Ogg.main args
        "Zip"  -> Zip.main args        
        "Bzip" -> Bzip.main args
        "Tar"  -> Tar.main args
        "Tga"  -> Tga.main args
        "Jpeg" -> Jpeg.main args
        "Tiff" -> Tiff.main args
        "Xml"  -> Xml.main args
        "Html" -> Html.main args
        "Js"   -> Js.main args
        "Png"  -> Png.main args
        "Pnm"  -> Pnm.main args
        "Svg"  -> SimpleSvg.main args
        "BS"  -> ByteString.main args

-- | Just checks that the command and the action are executables in the current
-- system
safetyChecks :: MainArgs -> IO ()
safetyChecks args = do
    let cmd = findCmd args
    cmdex <- findExecutable cmd
    unless (isJust cmdex) (die $ "The command \"" ++ cmd ++ "\" is not present.")
    let act = findAct args
    actx <- findExecutable act
    unless (isJust actx) (die $ "The action \"" ++ act ++ "\", cannot continue.") 
        

main = do
    interface <- cli
    runApp interface dispatch
