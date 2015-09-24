{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Graphics.Svg.PathParser( transformParser
                              , command
                              , viewBoxParser
                              , pointData
                              , serializePoints
                              , serializeCommand
                              , serializeCommands
                              , serializeViewBox
                              ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( (<*>), (<*), (*>), (<$>), (<$) )
#endif

import Control.Applicative( (<|>) )
import Data.Scientific( toRealFloat )
import Data.Attoparsec.Text
    ( Parser
    , scientific
    , string
    , skipSpace
    , char
    )
import Data.Attoparsec.Combinator( option
                                 , sepBy
                                 , sepBy1 )

import Linear hiding ( angle, point )
import Graphics.Svg.Types
import qualified Data.Text as T
import Text.Printf( printf )

num :: Parser Double
num = realToFrac <$> (skipSpace *> plusMinus <* skipSpace)
  where doubleNumber :: Parser Double
        doubleNumber = toRealFloat <$> scientific

        plusMinus = negate <$ string "-" <*> doubleNumber
                 <|> string "+" *> doubleNumber
                 <|> doubleNumber

viewBoxParser :: Parser (Int, Int, Int, Int)
viewBoxParser = (,,,)
       <$> iParse <*> iParse <*> iParse <*> iParse
  where
    iParse = floor <$> num <* skipSpace

serializeViewBox :: (Int, Int, Int, Int) -> String
serializeViewBox (a, b, c, d) = printf "%d %d %d %d" a b c d

commaWsp :: Parser ()
commaWsp = skipSpace *> option () (string "," *> return ()) <* skipSpace

point :: Parser RPoint
point = V2 <$> num <* commaWsp <*> num

pointData :: Parser [RPoint]
pointData = point `sepBy` commaWsp

command :: Parser PathCommand
command =  (MoveTo OriginAbsolute <$ string "M" <*> pointList)
       <|> (MoveTo OriginRelative <$ string "m" <*> pointList)
       <|> (LineTo OriginAbsolute <$ string "L" <*> pointList)
       <|> (LineTo OriginRelative <$ string "l" <*> pointList)
       <|> (HorizontalTo OriginAbsolute <$ string "H" <*> coordList)
       <|> (HorizontalTo OriginRelative <$ string "h" <*> coordList)
       <|> (VerticalTo OriginAbsolute <$ string "V" <*> coordList)
       <|> (VerticalTo OriginRelative <$ string "v" <*> coordList)
       <|> (CurveTo OriginAbsolute <$ string "C" <*> manyComma curveToArgs)
       <|> (CurveTo OriginRelative <$ string "c" <*> manyComma curveToArgs)
       <|> (SmoothCurveTo OriginAbsolute <$ string "S" <*> pointPairList)
       <|> (SmoothCurveTo OriginRelative <$ string "s" <*> pointPairList)
       <|> (QuadraticBezier OriginAbsolute <$ string "Q" <*> pointPairList)
       <|> (QuadraticBezier OriginRelative <$ string "q" <*> pointPairList)
       <|> (SmoothQuadraticBezierCurveTo OriginAbsolute <$ string "T" <*> pointList)
       <|> (SmoothQuadraticBezierCurveTo OriginRelative <$ string "t" <*> pointList)
       <|> (EllipticalArc OriginAbsolute <$ string "A" <*> manyComma ellipticalArgs)
       <|> (EllipticalArc OriginRelative <$ string "a" <*> manyComma ellipticalArgs)
       <|> (EndPath <$ string "Z" <* commaWsp)
       <|> (EndPath <$ string "z" <* commaWsp)
    where pointList = point `sepBy1` commaWsp
          pointPair = (,) <$> point <* commaWsp <*> point
          pointPairList = pointPair `sepBy1` commaWsp
          coordList = num `sepBy1` commaWsp
          curveToArgs = (,,) <$> (point <* commaWsp)
                             <*> (point <* commaWsp)
                             <*> point
          manyComma a = a `sepBy1` commaWsp

          numComma = num <* commaWsp
          ellipticalArgs = (,,,,,) <$> numComma
                                   <*> numComma
                                   <*> numComma
                                   <*> (fmap (/= 0) numComma)
                                   <*> (fmap (/= 0) numComma)
                                   <*> point

serializePoint :: RPoint -> String
serializePoint (V2 x y) = printf "%g,%g" x y

serializePoints :: [RPoint] -> String
serializePoints = unwords . fmap serializePoint

serializeCoords :: [Coord] -> String
serializeCoords = unwords . fmap (printf "%g")

serializePointPair :: (RPoint, RPoint) -> String
serializePointPair (a, b) = serializePoint a ++ " " ++ serializePoint b

serializePointPairs :: [(RPoint, RPoint)] -> String
serializePointPairs = unwords . fmap serializePointPair

serializePointTriplet :: (RPoint, RPoint, RPoint) -> String
serializePointTriplet (a, b, c) =
    serializePoint a ++ " " ++ serializePoint b ++ " " ++ serializePoint c

serializePointTriplets :: [(RPoint, RPoint, RPoint)] -> String
serializePointTriplets = unwords . fmap serializePointTriplet

serializeCommands :: [PathCommand] -> String
serializeCommands = unwords . fmap serializeCommand

serializeCommand :: PathCommand -> String
serializeCommand p = case p of
  MoveTo OriginAbsolute points -> "M" ++ serializePoints points
  MoveTo OriginRelative points -> "m" ++ serializePoints points
  LineTo OriginAbsolute points -> "L" ++ serializePoints points
  LineTo OriginRelative points -> "l" ++ serializePoints points

  HorizontalTo OriginAbsolute coords -> "H" ++ serializeCoords coords
  HorizontalTo OriginRelative coords -> "h" ++ serializeCoords coords
  VerticalTo OriginAbsolute coords -> "V" ++ serializeCoords coords
  VerticalTo OriginRelative coords -> "v" ++ serializeCoords coords

  CurveTo OriginAbsolute triplets -> "C" ++ serializePointTriplets triplets
  CurveTo OriginRelative triplets -> "c" ++ serializePointTriplets triplets
  SmoothCurveTo OriginAbsolute pointPairs -> "S" ++ serializePointPairs pointPairs
  SmoothCurveTo OriginRelative pointPairs -> "s" ++ serializePointPairs pointPairs
  QuadraticBezier OriginAbsolute pointPairs -> "Q" ++ serializePointPairs pointPairs
  QuadraticBezier OriginRelative pointPairs -> "q" ++ serializePointPairs pointPairs
  SmoothQuadraticBezierCurveTo OriginAbsolute points -> "T" ++ serializePoints points
  SmoothQuadraticBezierCurveTo OriginRelative points -> "t" ++ serializePoints points
  EllipticalArc OriginAbsolute args -> "A" ++ serializeArgs args
  EllipticalArc OriginRelative args -> "a" ++ serializeArgs args
  EndPath -> "Z"
  where
    serializeArg (a, b, c, d, e, V2 x y) =
        printf "%g %g %g %d %d %g,%g" a b c (fromEnum d) (fromEnum e) x y
    serializeArgs = unwords . fmap serializeArg



transformParser :: Parser Transformation
transformParser = matrixParser
               <|> translationParser
               <|> scaleParser
               <|> rotateParser
               <|> skewYParser
               <|> skewXParser

functionParser :: T.Text -> Parser [Double]
functionParser funcName =
    string funcName *> skipSpace
                    *> char '(' *> skipSpace
                    *> num `sepBy1` commaWsp
                    <* skipSpace <* char ')' <* skipSpace

translationParser :: Parser Transformation
translationParser = do
  args <- functionParser "translate"
  return $ case args of
    [x] -> Translate x 0
    [x, y] -> Translate x y
    _ -> TransformUnknown

skewXParser :: Parser Transformation
skewXParser = do
  args <- functionParser "skewX"
  return $ case args of
    [x] -> SkewX x
    _ -> TransformUnknown

skewYParser :: Parser Transformation
skewYParser = do
  args <- functionParser "skewY"
  return $ case args of
    [x] -> SkewY x
    _ -> TransformUnknown


scaleParser :: Parser Transformation
scaleParser = do
  args <- functionParser "scale"
  return $ case args of
    [x] -> Scale x Nothing
    [x, y] -> Scale x (Just y)
    _ -> TransformUnknown

matrixParser :: Parser Transformation
matrixParser = do
  args <- functionParser "matrix"
  return $ case args of
    [a, b, c, d, e, f] ->
        TransformMatrix a b c d e f
    _ -> TransformUnknown

rotateParser :: Parser Transformation
rotateParser = do
  args <- functionParser "rotate"
  return $ case args of
    [angle] -> Rotate angle Nothing
    [angle, x, y] -> Rotate angle $ Just (x, y)
    _ -> TransformUnknown
{-
rotate(<rotate-angle> [<cx> <cy>]), which specifies a rotation by <rotate-angle> degrees about a given point.

If optional parameters <cx> and <cy> are not supplied, the rotation is about the origin of the current user coordinate system. The operation corresponds to the matrix [cos(a) sin(a) -sin(a) cos(a) 0 0].

If optional parameters <cx> and <cy> are supplied, the rotation is about the point (cx, cy). The operation represents the equivalent of the following specification: translate(<cx>, <cy>) rotate(<rotate-angle>) translate(-<cx>, -<cy>).

skewX(<skew-angle>), which specifies a skew transformation along the x-axis.

skewY(<skew-angle>), which specifies a skew transformation along the y-axis.
    -}
