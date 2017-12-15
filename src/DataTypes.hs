{-
DataTypes.hs

Created by Geoffrey Natin on 9/12/2017

This file defines different data types that represent what a game of Core Wars consist of.
The elements of the MARS environment (Programs, Instructions and their parts)

https://github.com/nating/core-wars/src/DataTypes.hs
-}

module DataTypes where
   
import Data.Map
import Control.Concurrent.STM
import Control.Concurrent
import System.Console.ANSI


--------------------- Op ---------------------

data Op = DAT
         |MOV
         |ADD
         |SUB
         |JMP
         |JMZ
         |JMN
         |DJN
         |CMP
         |SPL
         deriving(Show,Read,Eq)




--------------------- Field ---------------------

data Field = Direct Int
            |Indirect Int
            |Immediate Int
            |AutoDecrement Int
            deriving(Eq)

instance Show Field where
  show (Direct a)        = " "++(show a)
  show (Indirect a)      = "@"++(show a)
  show (Immediate a)     = "#"++(show a)
  show (AutoDecrement a) = "<"++(show a)




--------------------- Instruction ---------------------

data Instruction = OneFieldOp Op Field
                  |TwoFieldOp Op Field Field 
                  deriving(Eq)

instance Show Instruction where
  show (OneFieldOp o f)     = (show o)++" "++(show f)
  show (TwoFieldOp o f1 f2) = (show o)++" "++(show f1)++", "++(show f2)

getFieldValue :: Field -> Int
getFieldValue (Direct v)        = v
getFieldValue (Indirect v)      = v
getFieldValue (Immediate v)     = v
getFieldValue (AutoDecrement v) = v

getBField :: Instruction -> Field
getBField (OneFieldOp _ b)   = b
getBField (TwoFieldOp _ _ b) = b

updateBField :: Instruction -> Int -> Instruction
updateBField (OneFieldOp o (Direct b)) i          = (OneFieldOp o (Direct i))
updateBField (OneFieldOp o (Immediate b)) i       = (OneFieldOp o (Immediate i))
updateBField (OneFieldOp o (Indirect b)) i        = (OneFieldOp o (Indirect i))
updateBField (OneFieldOp o (AutoDecrement b)) i   = (OneFieldOp o (AutoDecrement i))
updateBField (TwoFieldOp o a (Direct b)) i        = (TwoFieldOp o a (Direct i))
updateBField (TwoFieldOp o a (Immediate b)) i     = (TwoFieldOp o a (Immediate i))
updateBField (TwoFieldOp o a (Indirect b)) i      = (TwoFieldOp o a (Indirect i))
updateBField (TwoFieldOp o a (AutoDecrement b)) i = (TwoFieldOp o a (AutoDecrement i))

getAField :: Instruction -> Field
getAField (OneFieldOp _ a)   = a
getAField (TwoFieldOp _ a _) = a


--------------------- Types ---------------------

type Program = [Instruction]

type Memory = Map Int Instruction

--Prints out each instruction from memory on a new line
printMemoryNice :: Memory -> Color -> IO ()
printMemoryNice m c = do
   setSGR [SetColor Foreground Vivid c]
   putStr $ ( fst $ mapAccum makeNiceMemoryString "\nState of Memory: \n" m )++"\n"

--This is a function that is passed to mapAccum to create the accumulating string that will represent the memory
makeNiceMemoryString :: String -> Instruction -> (String,Instruction)
makeNiceMemoryString s v = (s++(show v)++"\n",v)

type Mars = TVar Memory
