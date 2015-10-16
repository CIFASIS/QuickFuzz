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
import qualified TTF

import System.Console.ArgParser 
import Args

dispatch :: MainArgs -> IO ()
dispatch args = case (findFileType args) of

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
        "TTF"  -> TTF.main args
        "BS"  -> ByteString.main args
        

main = do
    interface <- cli
    runApp interface dispatch
