{-# LANGUAGE CPP                #-}

module Main where

import qualified Process

import qualified Tiff
import qualified Png
import qualified Jpeg
import qualified Bmp
import qualified Gif

#ifdef COMPLETE
import qualified Zip
import qualified Tga
import qualified Ogg
import qualified Tar
import qualified Xml
import qualified Html
import qualified Pnm
import qualified Gzip
import qualified Bzip
import qualified Js
--import qualified Sh
import qualified Svg
import qualified ID3
import qualified Pandoc

--import qualified MBox
import qualified Css
import qualified Dot
import qualified ByteString
import qualified Unicode
import qualified TTF
import qualified Wav
import qualified CPIO
import qualified MarkUp
import qualified Regex

--import qualified MarkUpSvg

import qualified URI
import qualified JSON

import qualified Http
--import qualified Dns

#endif

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

            "Bmp"  -> Process.main (Bmp.mencode,undefined) args b
            "Gif"  -> Process.main (Gif.mencode,Gif.mdecode) args b
            "Jpeg" -> Process.main (Jpeg.mencode,Jpeg.mdecode) args b
            "Png"  -> Process.main (Png.mencode,Png.mdecode) args b
            "Tiff" -> Process.main (Tiff.mencode,undefined)  args b

#ifdef COMPLETE
            "Dot"  -> Process.main (Dot.mencode,undefined)  args b
            "Ogg"  -> Process.main (Ogg.mencode,undefined)  args b
            "Zip"  -> Process.main (Zip.mencode,undefined)  args b
            "Bzip" -> Process.main (Bzip.mencode,undefined)  args b
            "Gzip" -> Process.main (Gzip.mencode,undefined)  args b
            "Tar"  -> Process.main (Tar.mencode,undefined)  args b
            "Tga"  -> Process.main (Tga.mencode,undefined)  args b
            "Xml"  -> Process.main (Xml.mencode,Xml.mdecode)  args b
            "Html" -> Process.main (Html.mencode,undefined)  args b

            --"Html" -> Process.main MarkUp.mencodeHtml args b
            --"XHtml" -> Process.main MarkUp.mencodeXml args b
            "Js"   -> Process.main (Js.mencode,undefined)  args b
            "ID3"   -> Process.main (ID3.mencode,undefined)  args b
            --"Sh"   -> Sh.main args b

            "Pnm"  -> Process.main (Pnm.mencode,undefined)  args b
            "Rtf"  -> Process.main (Pandoc.mencode_rtf,undefined)  args b
            "Docx"  -> Process.main (Pandoc.mencode_docx,undefined)  args b
            "Odt"  -> Process.main (Pandoc.mencode_odt,undefined)  args b

            "Svg"  -> Process.main (Svg.mencode,Svg.mdecode)  args b
            "TTF"  -> Process.main (TTF.mencode,undefined)  args b
            "CSS"  -> Process.main (Css.mencode,undefined)  args b
            "Wav"  -> Process.main (Wav.mencode,undefined)  args b
            "CPIO" -> Process.main (CPIO.mencode,undefined)  args b
            "Regex" -> Process.main (Regex.mencode,undefined)  args b

            --"MarkUp" -> Process.main MarkUp.mencodeHtml args b
            --"MarkUpSvg" -> Process.main MarkUpSvg.mencode args b

            "HttpReq" -> Process.netmain Http.mencode_req args b
            "HttpRes" -> Process.netmain Http.mencode_res args b

            --"Tftp" -> Tftp.main args b
            --"Dns" -> Process.netmain Dns.mencode args b
            "URI"   -> Process.main (URI.mencode,undefined) args b
            "JSON"   -> Process.main (JSON.mencode,undefined) args b

            --"MBox"   -> MBox.main args b
            "Unicode" -> Process.main (Unicode.mencode,undefined)  args b
            "BS"   -> Process.main (ByteString.bencode,undefined)  args b

#endif
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
