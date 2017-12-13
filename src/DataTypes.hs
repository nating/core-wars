{-
RedCode.hs

Created by Geoffrey Natin on 9/12/2017

https://github.com/nating/core-wars/src/RedCode.hs
-}

module DataTypes where

   
import Data.Map
import Control.Concurrent.STM

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
         deriving (Show,Read)

data Field = Direct Int
            |Indirect Int
            |Immediate Int
            |AutoDecrement Int
            deriving (Show,Read)

data Instruction = OneFieldOp Op Field
                  |TwoFieldOp Op Field Field --The B field here is not supposed to be able to be immediate. I must fix this some how...
                  |NoFieldOp Op
                  |Empty
                  deriving (Show,Read)

type Program = [Instruction]

type Memory = Map Int Instruction

type Mars = TVar Memory

--TODO: There are time limits on the programs