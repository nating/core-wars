{-
Render.hs

Created by Geoffrey Natin on 15/11/2017

https://github.com/nating/interactive-shape-server/src/Render.hs
-}

{-# LANGUAGE OverloadedStrings #-}

module Render where

import Shapes
import Styles
import Transforms
import Text.Blaze.Svg11 
import Text.Blaze.Svg11.Attributes

{-
`createSvgFromDrawing` sets the header for the Blaze-SVG and then adds
each shape from the `Drawing` to the SVG.
-}
createSvgFromDrawing :: Drawing -> Svg
createSvgFromDrawing x = docTypeSvg ! version "1.1" ! width "500" ! height "500" ! viewbox "0 0 50 50" $ do
      createSvgShapes x


{-
`createSvgShapes` takes a Drawing and creates Blaze-SVG shapes with 
transformations and styles for each shape in the drawing.

Each Blaze-SVG shape is of the form:
BlazeSvg {'!' BlazeAttribute}
(i.e. a shape followed by zero or more Blaze-SVG Attributes with the
(!) operator as a delimiter)

The (!) operator is used in Blaze-SVG to apply attributes to shapes.

Given a Drawing, `createSvgShapes` converts the Shapes to
BlazeSVGs and their Transforms and Styles to Blaze-SVG Attributes
and combines the lists of Attributes with their shape using the (!) 
operator.
-}
createSvgShapes :: [(Shape,Transform,Style)] -> Svg
createSvgShapes [(sh,tr,st)] = Prelude.foldl (!) (svgShape sh) ( (svgTransform (transformToAttributes tr)) : (Styles.style st) )
createSvgShapes (x:xs) = ( createSvgShapes [x] ) >> ( createSvgShapes xs )

{-
`svgShape` converts a Shape into a Blaze-SVG SVG.
-}
svgShape :: Shape -> Svg
svgShape Empty = rect
svgShape Circle = circle ! r "1"
svgShape Square = rect ! width "1" ! height "1"


{-
`svgTransform` converts a list of Blaze-SVG AttributeValues to Attributes
using the Blaze-SVG function `transform`, which creates a Blaze-SVG
Attribute from a Blaze-SVG Attribute value.
-}
svgTransform :: [AttributeValue] -> Attribute
svgTransform t = transform $ mconcat $ t


{-
A Drawing is a list of (Transform,Shape,Style) tuples.
-}
type Drawing = [(Shape,Transform,Style)]