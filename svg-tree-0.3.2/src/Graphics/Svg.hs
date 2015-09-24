{-# LANGUAGE CPP #-}
-- | Module providing basic input/output for the SVG document,
-- for document building, please refer to Graphics.Svg.Types.
module Graphics.Svg ( -- * Saving/Loading functions
                      loadSvgFile
                    , parseSvgFile
                    , xmlOfDocument
                    , saveXmlFile

                      -- * Manipulation functions
                    , cssApply
                    , cssRulesOfText
                    , applyCSSRules
                    , resolveUses

                      -- * Type definitions
                    , module Graphics.Svg.Types
                    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( (<$>) )
#endif

import Data.List( foldl' )
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Text as T
import Text.XML.Light.Input( parseXMLDoc )
import Text.XML.Light.Output( ppcTopElement, prettyConfigPP )
import Control.Lens

import Graphics.Svg.Types
import Graphics.Svg.CssTypes
import Graphics.Svg.CssParser( cssRulesOfText )
import Graphics.Svg.XmlParser

{-import Graphics.Svg.CssParser-}

-- | Try to load an svg file on disc and parse it as
-- a SVG Document.
loadSvgFile :: FilePath -> IO (Maybe Document)
loadSvgFile filename =
  parseSvgFile filename <$> B.readFile filename

-- | Parse an in-memory SVG file
parseSvgFile :: FilePath    -- ^ Source path/URL of the document, used
                            -- to resolve relative links.
             -> B.ByteString
             -> Maybe Document
parseSvgFile filename fileContent =
  parseXMLDoc fileContent >>= unparseDocument filename

-- | Save a svg Document on a file on disk.
saveXmlFile :: FilePath -> Document -> IO ()
saveXmlFile filePath =
    writeFile filePath . ppcTopElement prettyConfigPP . xmlOfDocument

cssDeclApplyer :: DrawAttributes -> CssDeclaration
               -> DrawAttributes
cssDeclApplyer value (CssDeclaration txt elems) =
   case lookup txt cssUpdaters of
     Nothing -> value
     Just f -> f value elems
  where
    cssUpdaters = [(T.pack $ _attributeName n, u) |
                            (n, u) <- drawAttributesList]

-- | Rewrite a SVG Tree using some CSS rules.
--
-- This action will propagate the definition of the
-- css directly in each matched element.
cssApply :: [CssRule] -> Tree -> Tree
cssApply rules = zipTree go where
  go [] = None
  go ([]:_) = None
  go context@((t:_):_) = t & drawAttr .~ attr'
   where
     matchingDeclarations =
         findMatchingDeclarations rules context
     attr = view drawAttr t
     attr' = foldl' cssDeclApplyer attr matchingDeclarations

-- | For every 'use' tag, try to resolve the geometry associated
-- with it and place it in the scene Tree. It is important to
-- resolve the 'use' tag before applying the CSS rules, as some
-- rules may apply some elements matching the children of "use".
resolveUses :: Document -> Document
resolveUses doc =
  doc { _elements = mapTree fetchUses <$> _elements doc }
  where
    fetchUses (UseTree useInfo _) = UseTree useInfo $ search useInfo
    fetchUses a = a

    search nfo = maybe Nothing geometryExtract found where
      found = M.lookup (_useName nfo) $ _definitions doc

    geometryExtract c = case c of
      ElementLinearGradient _ -> Nothing
      ElementRadialGradient _ -> Nothing
      ElementMask _ -> Nothing
      ElementClipPath _ -> Nothing
      ElementGeometry t -> Just t
      ElementPattern _ -> Nothing
      ElementMarker _ -> Nothing

-- | Rewrite the document by applying the CSS rules embedded
-- inside it.
applyCSSRules :: Document -> Document
applyCSSRules doc = doc
    { _elements = cssApply (_styleRules doc) <$> _elements doc }

