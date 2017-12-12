{-
RedCode.hs

Created by Geoffrey Natin on 9/12/2017

https://github.com/nating/core-wars/src/RedCode.hs
-}

module DataTypes where

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

data Field = Direct Integer
            |Indirect Integer
            |Immediate Integer
            |AutoDecrement Integer
            deriving (Show,Read)

data Instruction = OneFieldOp Op Field
                  |TwoFieldOp Op Field Field --The B field here is not supposed to be able to be immediate. I must fix this some how...
                  |NoFieldOp Op
                  |Empty
                  deriving (Show,Read)

type Program = [Instruction]

--TODO: There are time limits on the programs