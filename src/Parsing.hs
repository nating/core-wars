{-
Parsing.hs

Created by Geoffrey Natin on 9/12/2017

This file contains functions used to read in core wars programs from text files.

https://github.com/nating/core-wars/src/Parsing.hs
-}

module Parsing where

import System.IO  
import Data.Char
import Data.List.Utils as T
import DataTypes

-----------------------------------------PARSING--------------------------------------------

{-
    Each non-blank line of the input file is formatted as follows:
        * An opcode (typically in all-caps)
        * Some whitespace (separating the opcode and the first field)
        * The addressing mode for the A field (may be omitted if the mode is Direct)
        * The value for the A field (this, and the previous item, are omitted if the instruction does not use the A field)
        * A comma (optionally with some whitespace on either side to allow the programmer to format the instruction neatly)
        * The addressing mode for the B field
        * The value for the B field
        (this and the previous two items are omitted if the instruction does not use the B field [DAT,SPL,JMP])
        * Optionally any sequence of text begun with a semicolon and concluded by the end of the line
    Blank lines are permitted, and are ignored.
-}


parsePrograms :: [String] -> IO [Program]
parsePrograms x = sequence $ fmap parse x

parse :: String -> IO Program
parse s = do  
        contents <- readFile s
        return $ readLines $ lines contents

readLines :: [String] -> Program
readLines x = fmap readLine x 

readLine :: String -> Instruction
readLine s = readInstruction $ filter (not . null) $ words $ removeComments $ fmap toUpper s

readInstruction :: [String] -> Instruction
readInstruction [s0,s1,",",s2] = TwoFieldOp (read s0) (readField $ init s1) (readField s2)
readInstruction [s0,s1,s2]     = TwoFieldOp (read s0) (readField $ init s1) (readField s2)
readInstruction [s0,s1]        = OneFieldOp (read s0) (readField s1)
readInstruction []             = OneFieldOp DAT (Direct 0)

readField :: String -> Field
readField ('@':x) = Indirect (read x)
readField ('#':x) = Immediate (read x)
readField ('<':x) = AutoDecrement (read x)
readField ('$':x) = Direct (read x)
readField x       = Direct (read x)

removeComments :: String -> String
removeComments [] = []
removeComments s = head $ T.split ";" s