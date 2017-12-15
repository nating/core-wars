{-
DataTypes.hs

Created by Geoffrey Natin on 9/12/2017

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
                  |TwoFieldOp Op Field Field --TODO: The B field here is not supposed to be able to be immediate. I must fix this some how...
                  deriving(Eq)

instance Show Instruction where
  show (OneFieldOp o f)     = (show o)++" "++(show f)
  show (TwoFieldOp o f1 f2) = (show o)++" "++(show f1)++", "++(show f2)




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
