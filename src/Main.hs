{-# LANGUAGE CPP                #-}

module Main where

import qualified Process

#ifdef IMGS

import qualified Tga
import qualified Tiff
import qualified Png
import qualified Jpeg
import qualified Bmp
import qualified Gif
import qualified Pnm
import qualified Svg

#endif

#ifdef ARCHS

import qualified Zip
import qualified Tar
import qualified Gzip
import qualified Bzip
import qualified CPIO

#endif

#ifdef CODES

import qualified Xml
import qualified Html
import qualified Css
import qualified Js
import qualified Python
import qualified Dot
import qualified JSON
import qualified Regex

#endif

#ifdef DOCS

import qualified Pandoc
import qualified PS

#endif

#ifdef NET

import qualified URI
import qualified Http
--import qualified Dns

#endif

#ifdef MEDIA

import qualified Ogg
--import qualified Sh
import qualified ID3
import qualified TTF
import qualified Wav

#endif

--import qualified MBox
import qualified ByteString
import qualified Unicode

--import qualified MarkUpSvg


import System.Console.ArgParser
import System.Random
import Args
import Data.Maybe
import System.Directory 
import System.Exit
import Control.Monad
import Data.List.Split

fillArgs :: MainArgs -> IO (String -> MainArgs)
fillArgs args =
    case findFileName args of
        [] -> do
            sG <- getStdGen
            let fname = take 10 ((randomRs ('a','z') sG) :: String )
            return $ formatArgs (formatFileName args fname)
        _ -> return $ formatArgs args

dispatch :: MainArgs -> IO ()
dispatch arg = do
        args <- fillArgs arg
        let b = findPar arg
        safetyChecks arg 
        case findFileType arg of

#ifdef IMGS

            "Bmp"  -> Process.main (Bmp.mencode,undefined) args b
            "Gif"  -> Process.main (Gif.mencode,Gif.mdecode) args b
            "Jpeg" -> Process.main (Jpeg.mencode,Jpeg.mdecode) args b
            "Png"  -> Process.main (Png.mencode,Png.mdecode) args b
            "Tiff" -> Process.main (Tiff.mencode,undefined)  args b
            "Tga"  -> Process.main (Tga.mencode,undefined)  args b
            "Pnm"  -> Process.main (Pnm.mencode,undefined)  args b
            "Svg"  -> Process.main (Svg.mencode,Svg.mdecode)  args b

#endif

#ifdef ARCHS

            "Zip"  -> Process.main (Zip.mencode,undefined)  args b
            "Bzip" -> Process.main (Bzip.mencode,undefined)  args b
            "Gzip" -> Process.main (Gzip.mencode,undefined)  args b
            "Tar"  -> Process.main (Tar.mencode,undefined)  args b
            "CPIO" -> Process.main (CPIO.mencode,undefined)  args b

#endif


#ifdef CODES

            "Dot"  -> Process.main (Dot.mencode,undefined)  args b
            "Xml"  -> Process.main (Xml.mencode,Xml.mdecode)  args b
            "Html" -> Process.main (Html.mencode,undefined)  args b
            "Js"   -> Process.main (Js.mencode,undefined)  args b
            "Py"   -> Process.main (Python.mencode,undefined)  args b
            "CSS"  -> Process.main (Css.mencode,undefined)  args b
            "JSON"   -> Process.main (JSON.mencode,undefined) args b
            "Regex" -> Process.main (Regex.mencode,undefined)  args b
            --"Sh"   -> Sh.main args b

#endif

#ifdef DOCS
            "Rtf"  -> Process.main (Pandoc.mencode_rtf,undefined)  args b
            "Docx"  -> Process.main (Pandoc.mencode_docx,undefined)  args b
            "Odt"  -> Process.main (Pandoc.mencode_odt,undefined)  args b
            "PS"  -> Process.main (PS.mencode,undefined)  args b

#endif

#ifdef NET

            "HttpReq" -> Process.netmain Http.mencode_req args b
            "HttpRes" -> Process.netmain Http.mencode_res args b

            --"Tftp" -> Tftp.main args b
            --"Dns" -> Process.netmain Dns.mencode args b
            "URI"   -> Process.main (URI.mencode,undefined) args b
#endif

#ifdef MEDIA

            "Ogg"  -> Process.main (Ogg.mencode,undefined)  args b
            --"Html" -> Process.main MarkUp.mencodeHtml args b
            --"XHtml" -> Process.main MarkUp.mencodeXml args b
            "ID3"   -> Process.main (ID3.mencode,undefined)  args b

            "TTF"  -> Process.main (TTF.mencode,undefined)  args b
            "Wav"  -> Process.main (Wav.mencode,undefined)  args b

#endif

            --"MarkUp" -> Process.main MarkUp.mencodeHtml args b
            --"MarkUpSvg" -> Process.main MarkUpSvg.mencode args b

            --"MBox"   -> MBox.main args b
            "Unicode" -> Process.main (Unicode.mencode,undefined)  args b
            "BS"   -> Process.main (ByteString.bencode,undefined)  args b

            _      -> print "Unsupported Type"

-- | Just checks that the command and the action are executables in the current
-- system
safetyChecks :: MainArgs -> IO ()
safetyChecks args = do
    return ()
    --cmdex <- findExecutable cmd
    --unless (isJust cmdex) (die $ "The command \"" ++ cmd ++ "\" is not present.")
    --let act = findAct args
    --actx <- findExecutable act
    --unless (isJust actx) (die $ "The action \"" ++ act ++ "\" cannot be done.")
        
main = do
    interface <- cli
    runApp interface dispatch
