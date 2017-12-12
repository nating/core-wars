{-
Styles.hs

Created by Geoffrey Natin on 15/11/2017

https://github.com/nating/interactive-shape-server/src/Styles.hs
-}

module Styles where

import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

import Color

{-
A Style is a style or composition of styles that
can be applied to a `Shape` (see Shapes.hs).

Each type of style corresponds to an SVG style with the
exception of `StyleCompose Style Style`.

The `StyleCompose Style Style` type of Style gives users the option 
of specifying multiple styles for a single shape.
-}
data Style = Fill Color
           | FillOpacity Double
           | StrokeWidth Double
           | StrokeColor Color
           | StyleCompose Style Style
             deriving (Show,Read)

{-
`style` converts a Style to a list of Blaze-SVG Attributes.
This is so that Styles can be used in the application of 
styles to Blaze-SVGs shapes.
-}
style :: Style -> [S.Attribute]
style (Fill c)             = [A.fill $ S.stringValue (show c)]
style (FillOpacity d)      = [A.fillOpacity $ S.stringValue (show d)]
style (StrokeWidth d)      = [A.strokeWidth $ S.stringValue (show d)]
style (StrokeColor c)      = [A.stroke $ S.stringValue (show c)]
style (StyleCompose s0 s1) = style s0 ++ style s1