{-
Transforms.hs

Created by Geoffrey Natin on 15/11/2017

https://github.com/nating/interactive-shape-server/src/Transforms.hs
-}

module Transforms where

import qualified Text.Blaze.Svg11 as S

{-
A Transform is a transformation or composition of transformations that
can be applied to a `Shape` (see Shapes.hs).

Each type of transform corresponds to an SVG transformation with the
exception of the `Identity` and `Compose Transform Transform` types of 
Transform.

The language for describing shapes, their transformations and their styles
requires that there is some input transformation. The `Identity` type of 
transform gives users the option of not performing any transform on a
shape.

The `Compose Transform Transform` type of Transform gives users the option 
of specifying multiple transformations that they would like to be applied 
to a single shape.
-}
data Transform = Identity
               | Translate Double Double
               | Scale Double Double
               | Rotate Double
               | SkewX Double
               | SkewY Double
               | Compose Transform Transform
                 deriving (Show,Read)

{-
`transformToAttributes` creates Blaze-SVG AttributeValues from Transforms
so that they can be used to create an SVG.

`transformToAttributes` returns a list of AttributeValues because
the composition of Transforms will produce multiple Attributes for an
SVG.
-}
transformToAttributes :: Transform -> [S.AttributeValue]
transformToAttributes Identity           = [S.scale 1 1]
transformToAttributes (Translate tx ty)  = [S.translate tx ty]
transformToAttributes (Scale sx sy)      = [S.scale sx sy]
transformToAttributes (Rotate angle)     = [S.rotate angle]
transformToAttributes (SkewX d)          = [S.skewX d]
transformToAttributes (SkewY d)          = [S.skewY d]
transformToAttributes (Compose t1 t2)    = transformToAttributes t2 ++ transformToAttributes t1
