{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances, TupleSections, OverloadedStrings #-}

module MarkUpSvg where

import Args
import Test.QuickCheck
import Check

import Data.Binary( Binary(..), encode )

import qualified Data.ByteString.Lazy.Char8 as LC8

--import Text.Blaze.Internal
import Text.Blaze.Svg.Renderer.String
import Text.Blaze.Svg as S
import Text.Blaze.Svg11 as S11
import Text.Blaze.Svg11.Attributes as S11A

instance Arbitrary Path where
    arbitrary = do
        a1 <- (arbitrary :: Gen Float)
        a2 <- (arbitrary :: Gen Float)
        a3 <- (arbitrary :: Gen Float)
        a4 <- (arbitrary :: Gen Float)
        a5 <- (arbitrary :: Gen Float)
        a6 <- (arbitrary :: Gen Float)
        a7 <- (arbitrary :: Gen Float)
        oneof $ Prelude.map return
            [
                m a1 a2 , mr a1 a2
                , S.z
                , l a1 a2, lr a1 a2, h a1, hr a1, v a1, vr a1
                , c a1 a2 a3 a4 a5 a6
                , cr a1 a2 a3 a4 a5 a6
                , s a1 a2 a3 a4, sr a1 a2 a3 a4
                , q a1 a2 a3 a4
                , qr a1 a2 a3 a4, t a1 a2, tr a1 a2
                , aa a1 a2 a3 a4 a5 a6 a7
                , ar a1 a2 a3 a4 a5 a6 a7
            ]


svgGen :: Gen S11.AttributeValue
svgGen = do
    p <- arbitrary
    a1 <- (arbitrary :: Gen Float)
    a2 <- (arbitrary :: Gen Float)
    a3 <- (arbitrary :: Gen Float)
    a4 <- (arbitrary :: Gen Float)
    a5 <- (arbitrary :: Gen Float)
    a6 <- (arbitrary :: Gen Float)
    oneof $ Prelude.map return
             [ mkPath p
             , translate a1 a2, S.rotate a1
             , rotateAround a1 a2 a3
             , S.scale a1 a2
             , skewX a1, skewY a1
             , matrix a1 a2 a3 a4 a5 a6 ]

svgAttr :: Gen Attribute
svgAttr = do
    av <- svgGen
    oneof $ Prelude.map (\f -> return $ f av)
        [
            accentHeight
                , accumulate
                , additive
                , alignmentBaseline
                , alphabetic
                , amplitude
                , arabicForm
                , ascent
                , attributename
                , attributetype
                , azimuth
                , basefrequency
                , baseprofile
                , baselineShift
                , bbox
                , begin
                , bias
                , by
                , calcmode
                , capHeight
                , class_
                , clip
                , clipPath
                , clipRule
                , clippathunits
                , color
                , colorInterpolation
                , colorInterpolationFilters
                , S11A.colorProfile
                , colorRendering
                , contentscripttype
                , contentstyletype
                , S11A.cursor
                , cx
                , cy
                , d
                , descent
                , diffuseconstant
                , direction
                , display
                , divisor
                , dominantBaseline
                , dur
                , dx
                , dy
                , edgemode
                , elevation
                , enableBackground
                , end
                , exponent_
                , externalresourcesrequired
                , fill
                , fillOpacity
                , fillRule
                , S11A.filter_
                , filterres
                , filterunits
                , floodColor
                , floodOpacity
                , fontFamily
                , fontSize
                , fontSizeAdjust
                , fontStretch
                , fontStyle
                , fontVariant
                , fontWeight
                , format
                , from
                , fx
                , fy
                , g1
                , g2
                , glyphName
                , glyphOrientationHorizontal
                , glyphOrientationVertical
                , S11A.glyphref
                , gradienttransform
                , gradientunits
                , hanging
                --, height
                , horizAdvX
                , horizOriginX
                , horizOriginY
                , id_
                , ideographic
                , imageRendering
                , in_
                , in2
                , intercept
                , k
                , k1
                , k2
                , k3
                , k4
                , kernelmatrix
                , kernelunitlength
                , kerning
                , keypoints
                , keysplines
                , keytimes
                , lang
                , lengthadjust
                , letterSpacing
                , lightingColor
                , limitingconeangle
                , local
                , markerEnd
                , markerMid
                , markerStart
                , markerheight
                , markerunits
                , markerwidth
                , S11A.mask
                , maskcontentunits
                , maskunits
                , mathematical
                , max_
                , media
                , method
                , min_
                , mode
                , name
                , numoctaves
                , offset
                , onabort
                , onactivate
                , onbegin
                , onclick
                , onend
                , onerror
                , onfocusin
                , onfocusout
                , onload
                , onmousedown
                , onmousemove
                , onmouseout
                , onmouseover
                , onmouseup
                , onrepeat
                , onresize
                , onscroll
                , onunload
                , onzoom
                , opacity
                , operator
                , order
                , orient
                , orientation
                , origin
                , overflow
                , overlinePosition
                , overlineThickness
                , panose1
                , S11A.path
                , pathlength
                , patterncontentunits
                , patterntransform
                , patternunits
                , pointerEvents
                , points
                , pointsatx
                , pointsaty
                , pointsatz
                , preservealpha
                , preserveaspectratio
                , primitiveunits
                , r
                , radius
                , refx
                , refy
                , renderingIntent
                , repeatcount
                , repeatdur
                , requiredextensions
                , requiredfeatures
                , restart
                , result
                , S11A.rotate
                , rx
                , ry
                , S11A.scale
                , seed
                , shapeRendering
                , slope
                , spacing
                , specularconstant
                , specularexponent
                , spreadmethod
                , startoffset
                , stddeviation
                , stemh
                , stemv
                , stitchtiles
                , stopColor
                , stopOpacity
                , strikethroughPosition
                , strikethroughThickness
                , S11A.string
                , stroke
                , strokeDasharray
                , strokeDashoffset
                , strokeLinecap
                , strokeLinejoin
                , strokeMiterlimit
                , strokeOpacity
                , strokeWidth
                , S11A.style
                , surfacescale
                , systemlanguage
                , tablevalues
                , target
                , targetx
                , targety
                , textAnchor
                , textDecoration
                , textRendering
                , textlength
                , S11A.title
                , to
                , transform
                , type_
                , u1
                , u2
                , underlinePosition
                , underlineThickness
                , unicode
                , unicodeBidi
                , unicodeRange
                , unitsPerEm
                , vAlphabetic
                , vHanging
                , vIdeographic
                , vMathematical
                , values
                , vertAdvY
                , vertOriginX
                , vertOriginY
                , viewbox
                , viewtarget
                , visibility
                --, width
                , widths
                , wordSpacing
                , writingMode
                , x
                , xHeight
                , x1
                , x2
                , xchannelselector
                , xlinkActuate
                , xlinkArcrole
                , xlinkHref
                , xlinkRole
                , xlinkShow
                , xlinkTitle
                , xlinkType
                , xmlBase
                , xmlLang
                , xmlSpace
                , y
                , y1
                , y2
                , ychannelselector
                , S11A.z
                , zoomandpan
        ]

instance Arbitrary Svg where
    arbitrary = do
        gsvg <- arbitrary
        attr <- svgAttr 
        frequency $ (
            [ (20, return (gsvg ! attr)) ]
            ++
             Prelude.map (\x -> (1, return x))
            [
                docType, docTypeSvg gsvg
                , a gsvg
                , altglyph
                , altglyphdef
                , altglyphitem
                , animate
                , animatecolor
                , animatemotion
                , animatetransform
                , circle
                , clippath gsvg
                , S11.colorProfile
                , S11.cursor
                , defs gsvg
                , desc
                , ellipse
                , feblend
                , fecolormatrix
                , fecomponenttransfer
                , fecomposite
                , feconvolvematrix
                , fediffuselighting
                , fedisplacementmap
                , fedistantlight
                , feflood
                , fefunca
                , fefuncb
                , fefuncg
                , fefuncr
                , fegaussianblur
                , feimage
                , femerge
                , femergenode
                , femorphology
                , feoffset
                , fepointlight
                , fespecularlighting
                , fespotlight
                , fetile
                , feturbulence
                , S11.filter_
                , font
                , fontFace
                , fontFaceFormat
                , fontFaceName
                , fontFaceSrc
                , fontFaceUri
                , foreignobject
                , g gsvg
                , glyph gsvg
                , S11.glyphref
                , hkern
                , image
                , line
                , lineargradient gsvg
                , marker gsvg
                , S11.mask gsvg
                , metadata
                , missingGlyph gsvg
                , mpath
                , S11.path
                , pattern gsvg
                , polygon
                , polyline
                , radialgradient gsvg
                , rect
                , script
                , set
                , stop
                , S11.style
                , svg gsvg
                , switch gsvg
                , symbol gsvg
                , text_ gsvg
                , textpath
                , S11.title
                , tref
                , tspan
                , use
                , view
                , vkern
            ])

instance Show Svg where
    show = renderSvg

type Viewport = (Positive Float, Positive Float, Positive Float, Positive Float)

type MSvg = (Svg, Positive Float, Positive Float, Viewport)

prepare :: MSvg -> Svg
prepare (x, Positive w, Positive h, (Positive x0, Positive y0, Positive x1, Positive y1)) =
   (S11.docTypeSvg ! S11A.version "1.1" ! width (toValue w) ! height (toValue h) ! S11A.viewbox (toValue (show x0 ++ " " ++ show y0 ++ show x1 ++ show y1))) x
    
mencode :: MSvg -> LC8.ByteString
mencode x = LC8.pack $ renderSvg (prepare x)
