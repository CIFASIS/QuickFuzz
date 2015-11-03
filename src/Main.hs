{-# LANGUAGE CPP                #-}

module Main where

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
import qualified SimpleSvg
import qualified Svg
--import qualified MBox
import qualified Css
import qualified Dot
import qualified ByteString
import qualified TTF
import qualified Wav

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
            "Bmp"  -> Bmp.main args b
            "Gif"  -> Gif.main args b
            "Jpeg" -> Jpeg.main args b
            "Png"  -> Png.main args b
            "Tiff" -> Tiff.main args b

#ifdef COMPLETE
            "Dot"  -> Dot.main args b
            "Ogg"  -> Ogg.main args b
            "Zip"  -> Zip.main args b
            "Bzip" -> Bzip.main args b
            "Gzip" -> Gzip.main args b
            "Tar"  -> Tar.main args b
            "Tga"  -> Tga.main args b
            "Xml"  -> Xml.main args b
            "Html" -> Html.main args b
            "Js"   -> Js.main args b
            "Pnm"  -> Pnm.main args b
            "Svg"  -> Svg.main args b
            "TTF"  -> TTF.main args b
            "CSS"  -> Css.main args b
            "Wav"  -> Wav.main args b

            --"MBox"   -> MBox.main args b
            "BS"   -> ByteString.main args b
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
