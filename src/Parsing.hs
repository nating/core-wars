import System.IO  
import Data.Char
import Data.List.Utils as T
import Control.Monad
import DataTypes
import System.Random
import Data.Time.Clock
import Data.Map

fetty = parsePrograms ["program1.txt","program2.txt"]

wap = "hello"

-------------------------------INSTRUCTIONS---------------------------------------------

performInstruction :: Instruction -> IO ()
performInstruction (OneFieldOp o f) = performOne o f
performInstruction (TwoFieldOp o f1 f2) = performTwo o f1 f2
--performInstruction (NoFieldOp o) = performNo o

performOne :: Op -> Field -> IO ()
performOne DAT _ = print "Kill Process"
performOne jmp a = print jmp
performOne spl a = print spl

performTwo :: Op -> Field -> Field -> IO ()
performTwo o f1 f2 = print o



-------------------------------REDCODE---------------------------------------------

{-

wap :: Int -> (Int, StdGen)
wap s = do
         seed <- (read <$> formatTime defaultTimeLocale "%s" <$> getCurrentTime)
	     return $ getRandomNumber (0,8000) seed

getRandomNumber :: (Int,Int) -> Int -> (Int,StdGen)
getRandomNumber (lo,hi) seed = randomR (lo,hi) (mkStdGen seed)

-}

setMarsMemory :: Int -> [Program] -> Map Int Instruction
setMarsMemory n p = union ( unions $ fmap createProgramMap p) $ createDatMemory n


createDatMemory :: Int -> Map Int Instruction
createDatMemory n = fromList $ zipWith makePair [1..n] $ replicate n (OneFieldOp DAT (Immediate 0))
    where makePair a b = (a,b)

createProgramMap :: Program -> Map Int Instruction
createProgramMap p = fromList $ zipWith makePair [1..] $ p
    where makePair a b = (a,b)


---------------------------------PARSING--------------------------------------------

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