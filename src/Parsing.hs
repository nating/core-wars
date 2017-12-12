import System.IO  
import Data.Char
import Data.List.Utils as T
import Control.Monad
import DataTypes
import System.Random
import Data.Time.Clock

fetty = parsePrograms ["program1.txt","program2.txt"]

wap :: Int -> (Int, StdGen)
wap s = do
         seed <- (read <$> formatTime defaultTimeLocale "%s" <$> getCurrentTime)
	     return $ getRandomNumber (0,8000) seed

--------------------------------------------------------------------------------------

getRandomNumber :: (Int,Int) -> Int -> (Int,StdGen)
getRandomNumber (lo,hi) seed = randomR (lo,hi) (mkStdGen seed)


runPrograms :: [Instruction]
runPrograms = replicate 8000 (OneFieldOp DAT (Immediate 0))


--------------------------------------------------------------------------------------

parsePrograms :: [String] -> IO [Program]
parsePrograms x = sequence $ fmap parse x

parse :: String -> IO Program
parse s = do  
        contents <- readFile s
        return $ readInstructions $ lines contents

readInstructions :: [String] -> Program
readInstructions instructionLines = fmap readInstruction instructionLines 

readInstruction :: String -> Instruction
readInstruction s = readParts $ words $ removeComments $ fmap toUpper s

readParts :: [String] -> Instruction
readParts [s0,s1,",",s2] = TwoFieldOp (read s0) (readField $ init s1) (readField s2)
readParts [s0,s1,s2] = TwoFieldOp (read s0) (readField $ init s1) (readField s2)
readParts [s0,s1]    = OneFieldOp (read s0) (readField s1)
readParts [s0]       = NoFieldOp (read s0)
readParts []         = Empty

readField :: String -> Field
readField ('@':xs) = Indirect (read xs)
readField ('#':xs) = Immediate (read xs)
readField ('<':xs) = AutoDecrement (read xs)
readField ('$':xs) = Direct (read xs)
readField s = Direct (read s)

removeComments :: String -> String
removeComments s = head $ T.split ";" s