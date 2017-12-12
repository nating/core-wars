{-
Main.hs

Created by Geoffrey Natin on 15/11/2017

https://github.com/nating/interactive-shape-server/src/Main.hs
-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parsing
import DataTypes
import Data.Seq





runPrograms :: [String] -> IO ()
runPrograms s = do
                programs <- parsePrograms s
                memory <- replicate 8000 (OneFieldOp DAT Immediate 0)
                randomNumber <- randomR (0,8000) (mkStdGen 100)

{-
    We want the main to 
         take a list of names of files, 
         parse each file into a 'Program', 
         create a MARS which is a list of instructions,
         place each program into random places in the MARS,
         create a thread for each program with each thread having a pointer to the address of its current instruction to be executed,
         start the round robin,
}

-}

main = runPrograms


