{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module Dot where

import Test.QuickCheck 
import Data.Binary( Binary(..), encode )
import Data.DeriveTH

import DeriveArbitrary
import ByteString
import Vector

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

import Language.Dot.Syntax
import Language.Dot.Pretty

import Data.Char (chr)
--import Data.List.Split

--genName :: Gen String
--genName = listOf1 validChars :: Gen String
--  where validChars = chr <$> choose (97, 122)

--instance {-# OVERLAPPING #-} Arbitrary String where
--   arbitrary = genName

-- $(devArbitrary ''Graph)

class Fixable a where
  fixUp :: a -> Gen a

instance Fixable Id where
  fixUp = readableFix -- return . identifierFix

instance Fixable Attribute where
  fixUp = attrFix

instance Fixable Statement where
  fixUp = stmtFix

--case independent?
isReservedWord :: String -> Bool
isReservedWord w = elem w keywords
  where keywords = ["node", "edge", "graph", "digraph", "subgraph", "strict"]

attributes :: [String]
attributes = ["Damping", "K", "URL", "_background", "area", "arrowhead", "arrowsize", "arrowtail", "bb", "bgcolor",
              "center", "charse", "clusterrank", "color", "colorscheme", "comment", "compound", "concentrate",
              "constraint", "decorate", "defaultdlist", "dim", "dimen", "dir", "diredgeconstraints", "distortion",
              "dpi", "edgeURL", "edgehref", "edgetarget", "edgetooltip", "epsilon", "esep", "fillcolor", "fixedsize",
              "fontcolor", "fontname", "fontnames", "fontpath", "fontsize", "forcelabels", "gradientangle", "group",
              "headURL", "head_lp", "headclip", "headhref", "headlabel", "headport", "headtarget", "headtooltip",
              "height", "href", "id", "image", "imagepath", "imagescale", "inputscale", "label", "labelURL",
              "label_scheme", "labelangle", "labeldistance", "labelfloat", "labelfontcolor", "labelfontname",
              "labelfontsize", "labelhref", "labeljust", "labelloc", "labeltarget", "labeltooltip", "landscape",
              "layer", "layerlistsep", "layers", "layerselect", "layersep", "layout", "len", "levels", "levelgap",
              "lhead", "lheight", "lp", "ltail", "lwidth", "margin", "maxiter", "mclimit", "mindist", "mindist",
              "minlen", "mode", "model", "mosek", "nodesep", "nojustify", "normalize", "notranslate", "nslimit",
              "nslimit1", "ordering", "orientation", "outputorder", "overlap", "overlap_scaling", "overlap_shrink",
              "pack", "packmode", "pad", "page", "pagedir", "pencolor", "penwidth", "peripheries", "pin", "pos",
              "quadtree", "quantum", "rank", "rankdir", "ranksep", "ratio", "rects", "regular", "remincross",
              "repulsiveforce", "resolution", "root", "rotate", "rotation", "samehead", "sametail", "samplepoints",
              "scale", "searchsize", "sep", "shape", "shapefile", "showboxes", "sides", "size", "skew", "smoothing",
              "sortv", "splines", "start", "style", "stylesheet", "tailURL", "tail_lp", "tailclip", "tailhref",
              "taillabel", "tailport", "tailtarget", "tailtooltip", "target", "tooltip", "truecolor", "vertices",
              "viewpoint", "voro_margin", "weight", "width", "xdotversion", "xlabel", "xlp", "z"]

attrFix :: Attribute -> Gen Attribute
attrFix (AttributeSetTrue _) = do na <- elements attributes
                                  return $ AttributeSetTrue (NameId na)
attrFix (AttributeSetValue _ v) = do na <- elements attributes
                                     return $ AttributeSetValue (NameId na) v

stmtFix :: Statement -> Gen Statement
stmtFix (EdgeStatement es as) = EdgeStatement <$> edgeFix es <*> return as
stmtFix (AssignmentStatement _ b) = do na <- elements attributes
                                       return $ AssignmentStatement (NameId na) b
stmtFix x = return x

edgeFix :: [Entity] -> Gen [Entity]
edgeFix [] = return []
edgeFix (e:es) = case e of
                    ENodeId et id -> case et of
                        NoEdge -> do fes <- edgeFix' es
                                     return (e:fes)
                        _      -> do fes <- edgeFix' es
                                     let ne = ENodeId NoEdge id
                                     return (ne:fes)
                    ESubgraph et s -> case et of
                        NoEdge -> do fes <- edgeFix' es
                                     return (e:fes)
                        _      -> do fes <- edgeFix' es
                                     let ne = ESubgraph NoEdge s
                                     return (ne:fes)

edgeFix' :: [Entity] -> Gen [Entity]
edgeFix' [] = return [] 
edgeFix' (x:xs) = case x of 
                   ENodeId et id -> case et of
                       NoEdge -> do fxs <- edgeFix' xs
                                    net <- elements [DirectedEdge, UndirectedEdge]
                                    let nx = ENodeId net id
                                    return (nx:fxs)
                       _     -> do fxs <- edgeFix' xs
                                   return (x:fxs)
                   ESubgraph et s -> case et of
                       NoEdge -> do fxs <- edgeFix' xs
                                    net <- elements [DirectedEdge, UndirectedEdge]
                                    let nx = ESubgraph net s
                                    return (nx:fxs)
                       _     -> do fxs <- edgeFix' xs
                                   return (x:fxs)                  

start :: [Char]
start = ['a'..'z'] ++ ['A'..'Z'] ++ ['_']

cont :: [Char]
cont = start ++ ['0'..'9']

readableFix :: Id -> Gen Id
readableFix (NameId _) = do s <- elements start
                            c <- shuffle cont
                            let nid = s:(take 2 c)
                                fid = if isReservedWord nid then '_':nid else nid
                            return (NameId fid)
readableFix (StringId _) = do s <- elements start
                              c <- shuffle cont
                              let nid = s:(take 2 c)
                              return (NameId nid)
readableFix x = return x --xml?

identifierFix :: Id -> Id
identifierFix (NameId s) = let fixStart c = if elem c start then c else '_'
                               fixContinue c = if elem c cont then c else '_'
                           in case s of
                             ""      -> NameId $ "_"
                             (st:c)  -> let fixed = (fixStart st):(map fixContinue c)
                                        in if isReservedWord fixed then NameId $ ('_':fixed) else NameId $ fixed
identifierFix x = x --xml?

-------------------------------------------------------------------------------------------------------------------

instance Arbitrary XmlName where
  arbitrary = sized go where
        go n = XmlName <$> resize (max 0 (n - 1)) arbitrary

instance Arbitrary XmlAttributeValue where
  arbitrary = sized go where
        go n = XmlAttributeValue <$> resize (max 0 (n - 1)) arbitrary

instance Arbitrary XmlAttribute where
  arbitrary = sized go where
        go n = XmlAttribute <$> resize (max 0 (n - 1)) arbitrary <*> resize (max 0 (n - 1)) arbitrary

instance Arbitrary Xml where
  arbitrary = sized go where
        go n = if (n <= 1) then oneof [XmlEmptyTag <$> resize (max 0 (n - 1)) arbitrary
                                                   <*> (listOf $ (resize (n `div` 10) arbitrary)),
                                       XmlText <$> resize (max 0 (n - 1)) arbitrary]
               else oneof [XmlEmptyTag <$> resize (max 0 (n - 1)) arbitrary
                                       <*> (listOf $ (resize (n `div` 10) arbitrary)),
                           XmlTag <$> resize (max 0 (n - 1)) arbitrary <*> (listOf $ (resize (n `div` 10) arbitrary))
                                  <*> (listOf $ (resize (n `div` 10) arbitrary)),
                           XmlText <$> resize (max 0 (n - 1)) arbitrary]

instance Arbitrary Id where
  arbitrary = sized go >>= fixUp where
        go n = oneof [NameId <$> resize (max 0 (n - 1)) arbitrary,
                      StringId <$> resize (max 0 (n - 1)) arbitrary,
                      IntegerId <$> resize (max 0 (n - 1)) arbitrary,
                      FloatId <$> resize (max 0 (n - 1)) arbitrary{-,
                      XmlId <$> resize (max 0 (n - 1)) arbitrary-}]

instance Arbitrary Attribute where
  arbitrary = sized go >>= fixUp where
        go n = oneof [AttributeSetTrue <$> resize (max 0 (n - 1)) arbitrary,
                      AttributeSetValue <$> resize (max 0 (n - 1)) arbitrary <*> resize (max 0 (n - 1)) arbitrary]

instance Arbitrary AttributeStatementType where
  arbitrary = elements [GraphAttributeStatement, NodeAttributeStatement, EdgeAttributeStatement]

instance Arbitrary Compass where
  arbitrary = elements [CompassN, CompassE, CompassS, CompassW, CompassNE, CompassNW, CompassSE, CompassSW]

instance Arbitrary EdgeType where
  arbitrary = elements [NoEdge, DirectedEdge, UndirectedEdge]

instance Arbitrary Port where
  arbitrary = sized go where
        go n = oneof [PortI <$> resize (max 0 (n - 1)) arbitrary <*> resize (max 0 (n - 1)) arbitrary,
                      PortC <$> resize (max 0 (n - 1)) arbitrary]

instance Arbitrary NodeId where
  arbitrary = sized go where
        go n = NodeId <$> resize (max 0 (n - 1)) arbitrary <*> resize (max 0 (n - 1)) arbitrary

instance Arbitrary Statement where
  arbitrary = sized go >>= fixUp where
        go n = frequency [(1, NodeStatement <$> resize (max 0 (n - 1)) arbitrary <*> (listOf $ (resize (n `div` 10) arbitrary))),
                          (5, EdgeStatement <$> (listOf $ (resize (n `div` 10) arbitrary)) <*> (listOf $ (resize (n `div` 10) arbitrary))),
                          (1, AttributeStatement <$> resize (max 0 (n - 1)) arbitrary <*> (listOf $ (resize (n `div` 10) arbitrary))),
                          (1, AssignmentStatement <$> resize (max 0 (n - 1)) arbitrary <*> resize (max 0 (n - 1)) arbitrary),
                          (1, SubgraphStatement <$> resize (max 0 (n - 1)) arbitrary)]

instance Arbitrary Subgraph where
  arbitrary = sized go where
        go n = oneof [NewSubgraph <$> resize (max 0 (n - 1)) arbitrary <*> (listOf $ (resize (n `div` 10) arbitrary)),
                      SubgraphRef <$> resize (max 0 (n - 1)) arbitrary]

instance Arbitrary Entity where
  arbitrary = sized go where
        go n = oneof [ENodeId <$> resize (max 0 (n - 1)) arbitrary <*> resize (max 0 (n - 1)) arbitrary,
                      ESubgraph <$> resize (max 0 (n - 1)) arbitrary <*> resize (max 0 (n - 1)) arbitrary]

instance Arbitrary GraphStrictness where
  arbitrary = elements [StrictGraph, UnstrictGraph]

instance Arbitrary GraphDirectedness where
  arbitrary = elements [DirectedGraph, UndirectedGraph]

instance Arbitrary Graph where
  arbitrary = sized go where
        go n = Graph <$> resize (max 0 (n - 1)) arbitrary <*> resize (max 0 (n - 1)) arbitrary <*> resize (max 0 (n - 1)) arbitrary  <*> (listOf $ (resize (n `div` 10) arbitrary))

mencode :: Graph -> L8.ByteString
mencode = L8.pack . renderDot 
