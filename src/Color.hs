{-
Color.hs

Created by Geoffrey Natin on 15/11/2017

https://github.com/nating/interactive-shape-server/src/Color.hs
-}

module Color where

{-
The 16 Color Names from the HTML 4.01 specification from 1999 are supported.
-}
data Color = Black
           | White
           | Blue
           | Green
           | Red
           | Silver
           | Gray
           | Yellow
           | Maroon
           | Olive
           | Lime
           | Aqua
           | Teal
           | Navy
           | Fuchsia
           | Purple
           deriving (Show,Read)
