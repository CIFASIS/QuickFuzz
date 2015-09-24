{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
-- | Define the types used to describes CSS elements
module Graphics.Svg.CssTypes
    ( CssSelector( .. )
    , CssSelectorRule
    , CssRule( .. )
    , CssDescriptor( .. )
    , CssDeclaration( .. )
    , CssElement( .. )

    , CssMatcheable( .. )
    , CssContext
    , Dpi
    , Number( .. )
    , serializeNumber
    , findMatchingDeclarations
    , toUserUnit
    , mapNumber
    , tserialize
    ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid( mconcat )
#endif

import Data.Monoid( (<>) )
import Data.List( intersperse )
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import Text.Printf

import Codec.Picture( PixelRGBA8( .. ) )
{-import Debug.Trace-}

-- | Alias describing a "dot per inch" information
-- used for size calculation (see toUserUnit).
type Dpi = Int

-- | Helper typeclass for serialization to Text.
class TextBuildable a where
    -- | Serialize an element to a text builder.
    tserialize :: a -> TB.Builder

-- | Describe an element of a CSS selector. Multiple
-- elements can be combined in a CssSelector type.
data CssDescriptor
  = OfClass T.Text    -- ^ .IDENT
  | OfName  T.Text    -- ^ IDENT
  | OfId    T.Text    -- ^ #IDENT
  | OfPseudoClass T.Text     -- ^ `:IDENT` (ignore function syntax)
  | AnyElem                  -- ^ '*'
  | WithAttrib T.Text T.Text -- ^ ``
  deriving (Eq, Show)

instance TextBuildable CssDescriptor where
  tserialize d = case d of
      OfClass c -> si '.' <> ft c
      OfName  n -> ft n
      OfId    i -> si '#' <> ft i
      OfPseudoClass c -> si '#' <> ft c
      AnyElem -> si '*'
      WithAttrib a b -> mconcat [si '[', ft a, si '=', ft b, si ']']
     where
      ft = TB.fromText
      si = TB.singleton

-- | Define complex selector.
data CssSelector
  = Nearby          -- ^ Correspond to the `+` CSS selector.
  | DirectChildren  -- ^ Correspond to the `>` CSS selectro.
  | AllOf [CssDescriptor] -- ^ Grouping construct, all the elements
                          -- of the list must be matched.
  deriving (Eq, Show)

instance TextBuildable CssSelector where
  tserialize s = case s of
      Nearby -> si '+'
      DirectChildren -> si '>'
      AllOf lst ->
        mconcat . intersperse (si ' ') $ map tserialize lst
    where
      si = TB.singleton

-- | A CssSelectorRule is a list of all the elements
-- that must be meet in a depth first search fashion.
type CssSelectorRule = [CssSelector]

-- | Represent a CSS selector and the different declarations
-- to apply to the matched elemens.
data CssRule = CssRule
    { -- | At the first level represent a list of elements
      -- to be matched. If any match is made, you can apply
      -- the declarations. At the second level
      cssRuleSelector :: ![CssSelectorRule]
      -- | Declarations to apply to the matched element.
    , cssDeclarations :: ![CssDeclaration]
    }
    deriving (Eq, Show)

instance TextBuildable CssRule where
  tserialize (CssRule selectors decl) =
      mconcat tselectors
                 <> ft " {\n"
                 <> mconcat (fmap tserializeDecl decl)
                 <> ft "}\n"
     where
      ft = TB.fromText
      tserializeDecl d = ft "  " <> tserialize d <> ft ";\n"
      tselectors =
          intersperse (ft ",\n") . fmap tserialize $ concat selectors

-- | Interface for elements to be matched against
-- some CssRule.
class CssMatcheable a where
  -- | For an element, tell its optional ID attribute.
  cssIdOf     :: a -> Maybe T.Text
  -- | For an element, return all of it's class attributes.
  cssClassOf  :: a -> [T.Text]
  -- | Return the name of the tagname of the element
  cssNameOf   :: a -> T.Text
  -- | Return a value of a given attribute if present
  cssAttribOf :: a -> T.Text -> Maybe T.Text

-- | Represent a zipper in depth at the first list
-- level, and the previous nodes at in the second
-- list level.
type CssContext a = [[a]]

isDescribedBy :: CssMatcheable a
              => a -> [CssDescriptor] -> Bool
isDescribedBy e = all tryMatch
  where
    tryMatch (OfClass t) = t `elem` cssClassOf e
    tryMatch (OfId    i) = cssIdOf e == Just i
    tryMatch (OfName  n) = cssNameOf e == n
    tryMatch (OfPseudoClass _) = False
    tryMatch (WithAttrib a v) = cssAttribOf e a == Just v
    tryMatch AnyElem = True

isMatching :: CssMatcheable a
           => CssContext a -> [CssSelector] -> Bool
isMatching = go where
  go  _ [] = True
  go []  _ = False
  go ((_ : near):upper) (Nearby : rest) = go (near:upper) rest
  go ((e:_):upper) (DirectChildren:AllOf descr:rest)
    | isDescribedBy e descr = go upper rest
  go _ (DirectChildren:_) = False
  go ((e:_):upper) (AllOf descr : rest)
    | isDescribedBy e descr = go upper rest
    | otherwise = False
  go (_:upper) selector = go upper selector

-- | Given CSS rules, find all the declaration to apply to the
-- element in a given context.
findMatchingDeclarations :: CssMatcheable a
                         => [CssRule] -> CssContext a -> [CssDeclaration]
findMatchingDeclarations rules context =
    concat [cssDeclarations rule
                    | rule <- rules
                    , selector <- cssRuleSelector rule
                    , isMatching context $ reverse selector ]

-- | Represent the content to apply to some
-- CSS matched rules.
data CssDeclaration = CssDeclaration
    { -- | Property name to change (like font-family or color).
      _cssDeclarationProperty :: T.Text
      -- | List of values
    , _cssDecarationlValues   :: [[CssElement]]
    }
    deriving (Eq, Show)

instance TextBuildable CssDeclaration where
  tserialize (CssDeclaration n elems) =
      mconcat $ ft n : ft ": " : intersperse (si ' ') finalElems
     where
      finalElems = map tserialize (concat elems)
      ft = TB.fromText
      si = TB.singleton


-- | Encode complex number possibly dependant to the current
-- render size.
data Number
  = Num Double       -- ^ Simple coordinate in current user coordinate.
  | Px Double        -- ^ With suffix "px"
  | Em Double        -- ^ Number relative to the current font size.
  | Percent Double   -- ^ Number relative to the current viewport size.
  | Pc Double
  | Mm Double        -- ^ Number in millimeters, relative to DPI.
  | Cm Double        -- ^ Number in centimeters, relative to DPI.
  | Point Double     -- ^ Number in points, relative to DPI.
  | Inches Double    -- ^ Number in inches, relative to DPI.
  deriving (Eq, Show)

-- | Helper function to modify inner value of a number
mapNumber :: (Double -> Double) -> Number -> Number
mapNumber f nu = case nu of
  Num n -> Num $ f n
  Px n -> Px $ f n
  Em n -> Em $ f n
  Percent n -> Percent $ f n
  Pc n -> Pc $ f n
  Mm n -> Mm $ f n
  Cm n -> Cm $ f n
  Point n -> Point $ f n
  Inches n -> Inches $ f n

-- | Encode the number to string which can be used in a
-- CSS or a svg attributes.
serializeNumber :: Number -> String
serializeNumber n = case n of
    Num c -> printf "%g" c
    Px c -> printf "%gpx" c
    Em cc -> printf "%gem" cc
    Percent p -> printf "%d%%" (floor $ 100 * p :: Int)
    Pc p -> printf "%gpc" p
    Mm m -> printf "%gmm" m
    Cm c -> printf "%gcm" c
    Point p -> printf "%gpt" p
    Inches i -> printf "%gin" i

instance TextBuildable Number where
   tserialize = TB.fromText . T.pack . serializeNumber

-- | Value of a CSS property.
data CssElement
    = CssIdent     !T.Text
    | CssString    !T.Text
    | CssReference !T.Text
    | CssNumber    !Number
    | CssColor     !PixelRGBA8
    | CssFunction  !T.Text ![CssElement]
    | CssOpComa
    | CssOpSlash
    deriving (Eq, Show)

instance TextBuildable CssElement where
  tserialize e = case e of
    CssIdent    n -> ft n
    CssString   s -> si '"' <> ft s <> si '"'
    CssReference r -> si '#' <> ft r
    CssNumber   n -> tserialize n
    CssColor  (PixelRGBA8 r g b _) ->
      ft . T.pack $ printf  "#%02X%02X%02X" r g b
    CssFunction t els -> mconcat $ ft t : si '(' : args ++ [si ')']
        where args = intersperse (ft ", ") (map tserialize els)
    CssOpComa -> si ','
    CssOpSlash -> si '/'
    where
      ft = TB.fromText
      si = TB.singleton

-- | This function replace all device dependant units to user
-- units given it's DPI configuration.
-- Preserve percentage and "em" notation.
toUserUnit :: Dpi -> Number -> Number
toUserUnit dpi = go where
  go nu = case nu of
    Num _ -> nu
    Px p -> go $ Num p
    Em _ -> nu
    Percent _ -> nu
    Pc n -> go . Inches $ (12 * n) / 72
    Inches n -> Num $ n * fromIntegral dpi
    Mm n -> go . Inches $ n / 25.4
    Cm n -> go . Inches $ n / 2.54
    Point n -> go . Inches $ n / 72

