{-
Warrior.hs

Created by Geoffrey Natin on 9/12/2017

This file contains functions used for making warriors take their turns in a game of core wars.

https://github.com/nating/core-wars/src/Warrior.hs
-}

module Warrior where

import DataTypes
import Parsing
import Data.Map as M
import Control.Concurrent
import Control.Concurrent.STM
import Data.Maybe
import System.Console.ANSI
import Data.Hashable

warriorWaitTime = 1000000 --1s. This slows the game down, so users can read what is going on in the game. Warriors wait before executing their next instruction.


------------------------------------WARRIOR TURNS--------------------------------------------

{-
    takeTurns:
        * atomically takes a warrior's turn
        * logs the instruction that the warrior just performed

        * if it has no more instruction pointers (tasks), then its thread is killed
-}
takeTurns :: [Int] -> Mars -> (MVar String) -> IO ()
takeTurns (ip:ips) marsEnv l = do
    (instruction,newIPList) <- atomically $ takeTurn (ip:ips) marsEnv
    letMeLog <- takeMVar l
    id <- myThreadId
    setSGR [SetColor Foreground Vivid (getThreadColor id)]
    putStrLn $ (show id)++" at location "++(show ip)++"  |   "++(show instruction)
    threadDelay warriorWaitTime
    putMVar l ""
    takeTurns newIPList marsEnv l
takeTurns [] _ _ = do
    id <- myThreadId
    killThread id

{-
    takeTurn:
        * takes the list of instruction pointers for the warrior's tasks

        * gets the MARS environment from the TVar
        * gets the instruction it is to perform
        * performs the instruction
        * updates the MARS environment

        * returns the instruction it performed and the updated list of instruction pointers for the warrior's tasks.
          the list of the instruction pointers for the warrior's tasks is updated based on what instruction was performed (DATs delete and entry/SPLs make a new one etc.)
-}
takeTurn :: [Int] -> Mars -> STM (Instruction,[Int])
takeTurn (ip:ips) marsTV = do
    memory                   <- (readTVar marsTV)
    let inst                  = getInstruction ip memory
    let (nextIp,newMars)      = performInstruction ip inst memory
    writeTVar marsTV newMars
    case inst of
        (OneFieldOp DAT _)   -> return (inst,ips)
        (TwoFieldOp DAT _ _) -> return (inst,ips)
        (OneFieldOp SPL _)   -> return (inst,(ips++[nextIp,addrAdd ip 1 memory]))
        _                    -> return (inst,(ips++[nextIp]))

--getInstruction returns the instruction at a certain index in the MARS environment
getInstruction :: Int -> Memory -> Instruction
getInstruction i mem = case M.lookup i mem of 
                         Just inst -> inst

-------------------------------INSTRUCTIONS---------------------------------------------

performInstruction :: Int -> Instruction -> Memory -> (Int,Memory)
performInstruction i (OneFieldOp o f) m     = (performOne i o f m,m)
performInstruction i (TwoFieldOp o f1 f2) m = performTwo i o f1 f2 m

performOne :: Int -> Op -> Field -> Memory -> Int
performOne _ DAT _ _ = -1
performOne i JMP a m = addrAdd i (getFieldValue a) m
performOne i SPL a m = addrAdd i (getFieldValue a) m

performTwo :: Int -> Op -> Field -> Field -> Memory -> (Int,Memory)
performTwo i MOV f1 f2 m = mov i f1 f2 m
performTwo i ADD f1 f2 m = add i f1 f2 m
performTwo i SUB f1 f2 m = sub i f1 f2 m
performTwo i JMZ f1 f2 m = jmz i f1 f2 m
performTwo i JMN f1 f2 m = jmn i f1 f2 m
performTwo i DJN f1 f2 m = djn i f1 f2 m
performTwo i CMP f1 f2 m = cmp i f1 f2 m


{-
    mov: 

    if the A-Field is immediate:
         Creates a DAT instruction in the instruction pointed at by the B-Field and the value of the A-field is placed in the new instructions B-field.
    else:
        Copies the complete contents of the location indicated by the A field into the location indicated by the B field.
-}
mov :: Int -> Field -> Field -> Memory -> (Int,Memory)
mov i (Immediate v) f2 m = (addrAdd i 1 m,newMem)
    where 
        targetAddress = getFinalAddress i f2 m
        newMem = M.insert targetAddress (OneFieldOp DAT (Direct v)) m
mov i f1 f2 m = (addrAdd i 1 m,newMem)
    where 
        targetAddress = getFinalAddress i f2 m
        overwritingInstructionIdx = addrAdd i (getFieldValue f1) m
        overwritingInstruction = fromJust $ M.lookup overwritingInstructionIdx m
        newMem = M.insert targetAddress overwritingInstruction m

{-
    add: Takes the contents of the A-Field and adds it from the contents of the B-Field of the instruction that the B-Field is pointing at.
-}
add :: Int -> Field -> Field -> Memory -> (Int,Memory)
add i f1 f2 m = (addrAdd i 1 m,newMem)
    where
        targetAddress = getFinalAddress i f2 m
        targetInstruction = fromJust $ M.lookup targetAddress m
        targetsPrevBFieldValue = getFieldValue $ getBField $ fromJust $ M.lookup targetAddress m
        addVal = getFieldValue f1
        updatedInstruction = updateBField targetInstruction (addrAdd targetsPrevBFieldValue addVal m)
        newMem = M.insert targetAddress updatedInstruction m

{-
    sub: Takes the contents of the A-Field and subtracts it from the contents of the B-Field of the instruction that the B-Field is pointing at.
-}
sub :: Int -> Field -> Field -> Memory -> (Int,Memory)
sub i f1 f2 m = (addrAdd i 1 m,newMem)
    where
        targetAddress = getFinalAddress i f2 m
        targetInstruction = fromJust $ M.lookup targetAddress m
        targetsPrevBFieldValue = getFieldValue $ getBField $ fromJust $ M.lookup targetAddress m
        subVal = getFieldValue f1
        updatedInstruction = updateBField targetInstruction (addrSub targetsPrevBFieldValue subVal m)
        newMem = M.insert targetAddress updatedInstruction m

{-
    jmz: Jumps to the instruction indicated by the A-Field if the value indicated by the B-Field is zero, otherwise it does nothing.  
-}
jmz :: Int -> Field -> Field -> Memory -> (Int,Memory)
jmz i f1 f2 m = if (getFieldValue f2)==0 
    then (addrAdd i (getAddress (getFieldValue f1) m) m,m)
    else (addrAdd i 1 m,m)

{-
    jmn: Jumps to the instruction indicated by the A-Field if the value indicated by the B-Field is non-zero, otherwise it does nothing.
-}
jmn :: Int -> Field -> Field -> Memory -> (Int,Memory)
jmn i f1 f2 m = if (getFieldValue f2) /= 0 
    then (addrAdd i (getAddress (getFieldValue f1) m) m,m)
    else (addrAdd i 1 m,m)

{-
    djn: Decrements the value indicated by the B-Field and then jumps to the instruction indicated by the A-Field if the number indicated by the B-Field has become non-zero.
-}
djn :: Int -> Field -> Field -> Memory -> (Int,Memory)
djn i f1 f2 m = if ((getFieldValue f2)-1) /= 0 
    then (addrAdd i (getFinalAddress i f1 m) m,m)
    else (addrAdd i 1 m,m)

{-
    cmp: Compares the values indicated by the A and B-Fields and skips the next instruction if they are not equal.
-}
cmp :: Int -> Field -> Field -> Memory -> (Int,Memory)
cmp i f1 f2 m = if (getFieldValue f1)==(getFieldValue f2)
    then (addrAdd i 1 m,m)
    else (addrAdd i 2 m,m)



--getFinalAddress is used to get the index of the target instruction of an instruction that is being executed by a warrior.
getFinalAddress :: Int -> Field -> Memory -> Int
getFinalAddress i (Direct v) m        = getAddress i m
getFinalAddress i (Immediate v) m     = getAddress i m
getFinalAddress i (Indirect v) m      = getAddress (addrAdd i v m) m
getFinalAddress i (AutoDecrement v) m = (getAddress (addrAdd i v m) m)-1 

--get Address is used to get the index of the instruction that the instruction at this index is pointing to.
getAddress :: Int -> Memory -> Int
getAddress i m = case M.lookup i m of 
                      Just (OneFieldOp _ f1)   -> addrAdd i (getFieldValue f1) m
                      Just (TwoFieldOp _ _ f2) -> addrAdd i (getFieldValue f2) m

--addrSub does address addition. All arithmetic is done modulo the length of memory
addrAdd :: Int -> Int -> Memory -> Int
addrAdd a b m = (a+b) `mod` (length m)

--addrSub does address subtraction. All arithmetic is done modulo the length of memory
addrSub :: Int -> Int -> Memory -> Int
addrSub a b m = abs $ (a-b) `mod` (length m)

--getThreadColor maps ThreadIds to colors
getThreadColor :: ThreadId -> Color
getThreadColor tid = case (hash (show tid)) `mod` 5 of
    0 -> Red  
    1 -> Yellow   
    2 -> Blue     
    3 -> Magenta  
    4 -> Cyan     

