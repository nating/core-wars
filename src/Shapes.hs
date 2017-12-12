{-
Shapes.hs

Created by Geoffrey Natin on 15/11/2017

https://github.com/nating/interactive-shape-server/src/Shapes.hs
-}

module Shapes where

{-
The three shapes; Empty, Circle and Square are supported.
-}
data Shape = Empty 
           | Circle 
           | Square
             deriving (Show,Read)