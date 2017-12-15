
module Parsing where

import System.IO  
import Data.Char
import Data.List.Utils as T
import Control.Monad
import DataTypes
import Control.Concurrent
import Data.Maybe

---------------------------------PARSING--------------------------------------------

parsePrograms :: [String] -> IO [Program]
parsePrograms x = sequence $ fmap parse x

parse :: String -> IO Program
parse s = do  
        contents <- readFile s
        return $ readInstructions $ lines contents

readInstructions :: [String] -> Program
readInstructions x = fmap readInstruction x 

readInstruction :: String -> Instruction
readInstruction s = readParts $ words $ removeComments $ fmap toUpper s

readParts :: [String] -> Instruction
readParts [s0,s1,",",s2] = TwoFieldOp (read s0) (readField $ init s1) (readField s2)
readParts [s0,s1,s2]     = TwoFieldOp (read s0) (readField $ init s1) (readField s2)
readParts [s0,s1]        = OneFieldOp (read s0) (readField s1)
readParts []             = OneFieldOp DAT (Direct 0)

readField :: String -> Field
readField ('@':x) = Indirect (read x)
readField ('#':x) = Immediate (read x)
readField ('<':x) = AutoDecrement (read x)
readField ('$':x) = Direct (read x)
readField x       = Direct (read x)

removeComments :: String -> String
removeComments s = head $ T.split ";" s